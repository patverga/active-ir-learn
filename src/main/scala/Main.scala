import cc.factorie.app.nlp.segment.{DeterministicSentenceSegmenter, DeterministicTokenizer}
import cc.factorie.app.nlp.{DocumentAnnotationPipeline, Document}
import cc.factorie.app.nlp.lexicon.StopWords
import edu.umass.ciir.strepsimur.galago.{GalagoParamTools, GalagoQueryBuilder, GalagoQueryLib, GalagoSearcher}
import edu.umass.cs.iesl.lffi.ner._
import org.lemurproject.galago.core.parse
import org.lemurproject.galago.core.retrieval.{ScoredPassage, ScoredDocument}

import org.lemurproject.galago.utility.Parameters

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.util.Random

/**
 * Created by pv on 1/12/15.
 */

object Main extends App {

//  val topic = "progressive rock bands 1970s"
//  val examples = Seq("Rush", "Genesis", "Emerson Lake and Palmer", "King Crimson")//,"Yes")
  val topic = "states in the United States"
  val examples = Seq("Massachusetts", "California", "Montana", "New York", "North Dakota")
//  val topic = "former presidents of the united states"
//  val examples = Seq("George Washington", "Bill Clinton", "Harry Truman", "George Bush", "Dwight D Eisenhower", "John F Kennedy", "Richard Nixon")

  val exampleRegex = examples.mkString(".*((", ")|(", ")).*").r
  println(exampleRegex)
  val exampleTerms = examples.map((_, 1.0)) //.mkString("#combine((", ")#combine( ", "))")


  val (searcher, params) = initializeGalago()
//  val (collectionRankings, passages) = getPassages
  val passages = examples.flatMap(getPassages)
  val r = train(passages)
  topK(r._1, r._2)


  def initializeGalago() : (GalagoSearcher, Parameters) = {
    val bookIndex = "/home/pv/index/robust04"
    val params = Params.robustTitleDoc
    val indexParam = new Parameters()
    indexParam.set("index", bookIndex)
    (GalagoSearcher(indexParam), params)
  }


  def getPassages(queryExample : String): Seq[String] = //(Seq[parse.Document], Seq[String]) =
  {
    val sdmQuery = s"#combine(${GalagoQueryBuilder.seqdep(topic).queryStr} ${GalagoQueryBuilder.seqdep(queryExample).queryStr})"
//    val sdmQuery = GalagoQueryBuilder.seqdep(exampleTerms.mkString(" ")).queryStr
//    val exampleQuery = GalagoQueryLib.buildWeightedCombine(Seq((sdmQuery, 0.33), (GalagoQueryLib.buildWeightedCombine(exampleTerms), 1 - 0.33)))

    // initial document retrieval
    val collectionRankings = searcher.retrieveScoredDocuments(sdmQuery, Some(params), 1000).map(d => searcher.pullDocument(d.documentName))
    params.set("passageQuery", true)
    params.set("passageSize", 50)
    params.set("passageShift", 25)
    params.set("casefold", false)
    params.set("working", collectionRankings.map(_.name).toList.asJava) // !! from a first pass!

    // extract passages from the documents
    val passages = searcher.retrieveScoredDocuments(sdmQuery, Some(params), 50).map(passDoc => {
        val pass = passDoc.asInstanceOf[ScoredPassage]
        val doc = searcher.pullDocumentWithTokens(passDoc.documentName)
        // TODO : hack because terms are all lower cased
        //      doc.terms.subList(pass.begin, pass.end).asScala.mkString(" ")
        val passageBuilder = new StringBuilder
        var i = pass.begin
        while (i < pass.end) {
          passageBuilder.append(doc.text.substring(doc.termCharBegin.get(i), doc.termCharEnd.get(i)))
          passageBuilder.append(" ")
          i += 1
        }
        passageBuilder.mkString
      }).filter(exampleRegex.pattern.matcher(_).matches())

//    println("PASSAGES")
//    passages.foreach(println(_))
    passages
//    (collectionRankings, passages)
  }


  def train(passages : Seq[String]) : (ConllForwardNer, Seq[Document], Seq[Document]) =
  {
    println("Training")
    val segmentPipeline = new DocumentAnnotationPipeline(Seq(DeterministicTokenizer, DeterministicSentenceSegmenter))
    val facDocs = Random.shuffle(passages.map(new Document(_)))
    facDocs.foreach(segmentPipeline.process)
//    val sentences = facDocs.flatMap(doc => doc.sentences)
    // TODO this only handles bigrams - also not robust
    facDocs.foreach(_.sentences.foreach(s => {
      s.tokens.sliding(2)
        .foreach(bigram => {
        // if bigram is in example, use that
        if(examples.contains(s"${bigram(0).string} ${bigram(1).string}")) {
          bigram(0).attr += new LabeledCustomNerTag(bigram(0), "B-BRAN")
          bigram(1).attr += new LabeledCustomNerTag(bigram(1), "L-BRAN")
        }
        // if not, is first token?
        else if (examples.contains(bigram(0).string))
          bigram(0).attr += new LabeledCustomNerTag(bigram(0), "U-BRAN")
        else "O"
          bigram(0).attr += new LabeledCustomNerTag(bigram(0), "O")
        // dont forget the last token of sentence
        if (bigram(1).isSentenceEnd)
          if (examples.contains(bigram(1).string))
            bigram(1).attr += new LabeledCustomNerTag(bigram(1), "U-BRAN")
          else
            bigram(1).attr += new LabeledCustomNerTag(bigram(1), "O")
      })
    }))

    val nerTrainer = new ConllForwardNer
    val mid = facDocs.size/2
    val test = facDocs take mid
    val train = facDocs.takeRight(mid)
    nerTrainer.train(train, test, iters=1)(Random)
    val r = (nerTrainer, train, test)
    r
  }

  def topK(model: ConllForwardNer, documents: Seq[Document]): Unit =
  {
    val scores = documents.flatMap(d => d.sentences.map(s => new LightweightNerTokenSpan(s.tokens))
      .flatMap(s => s.tokens.filterNot(t => StopWords.containsWord(t.string)).sliding(2)
      .map(bigram => {
        val s1 = model.predict(bigram(0))
        if (s1(4) > math.max(s1(1), s1(3))) (bigram(0).string, s1(4))
        else ( s"${bigram(0).string} ${bigram(1).string}", s1(1))
      }))).sortBy(-_._2)

    scores.filterNot(x => examples.contains(x._1)).foreach(println)

  }

}
