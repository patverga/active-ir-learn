import cc.factorie.app.nlp.embeddings.{WordEmbeddingModel, EmbeddingOpts}
import cc.factorie.app.nlp.phrase.BILOUChainChunker
import cc.factorie.app.nlp.pos.OntonotesForwardPosTagger
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

  val docResults = 1000
  val passageResults =1000
  val rand = new Random(69)
//    val topic = "small breeds of dogs"
//    val examples = Seq("chihuahua", "dachshund", "pug", "terrier", "beagle")
//    val topic = "states in the United States of America"
//    val examples = Seq(
//    "Massachusetts", "Montana", "Florida",
//    "South Carolina", "New York", "North Dakota"
//    )
//  val topic = "types of fruits"
//  val examples = Seq("apple", "orange" ,"banana", "lime", "kiwi")
    val topic = "United States of America politicians"
    val examples = Seq(
      "Bill Clinton", "John Kerry", "George Bush", "Ted Kennedy", "John Edwards", "Joe Lieberman"
//      "Kennedy", "Bush", "Clinton", "Jefferson", "Truman"
    )

  val exampleRegex = examples.mkString(".*((", ")|(", ")).*").r
  println(exampleRegex)
  val exampleTerms = examples.map((_, 1.0)) //.mkString("#combine((", ")#combine( ", "))")



  val (searcher, params) = initializeGalago()
  //  val (collectionRankings, passages) = getPassages
  val passages = examples.flatMap(getPassages)
  assert(passages.size>0, "no passages found")
  val (train, test) = labelData(passages, bigramLabels = true)

  val model = new ConllForwardNer
  model.train(train, test, iters=3)(rand)
    topK(model, test)

//  val model = new CustomNerChainCRF()
//  model.train(train++ test.take(test.size-1), Seq(test.last))(rand)



  def initializeGalago() : (GalagoSearcher, Parameters) = {
    val bookIndex = "/home/pv/index/robust04"
    val params = Params.robustTitleDoc
    val indexParam = new Parameters()
    indexParam.set("index", bookIndex)
    (GalagoSearcher(indexParam), params)
  }


  def getPassages(queryExample : String): Seq[(Double, String)] = //(Seq[parse.Document], Seq[String]) =
  {
    val sdmQuery = s"#combine(${GalagoQueryBuilder.seqdep(topic).queryStr} ${GalagoQueryBuilder.seqdep(queryExample).queryStr})"
    //    val sdmQuery = GalagoQueryBuilder.seqdep(exampleTerms.mkString(" ")).queryStr
    //    val exampleQuery = GalagoQueryLib.buildWeightedCombine(Seq((sdmQuery, 0.33), (GalagoQueryLib.buildWeightedCombine(exampleTerms), 1 - 0.33)))

    // initial document retrieval
    val collectionRankings = searcher.retrieveScoredDocuments(sdmQuery, Some(params), docResults).map(d => searcher.pullDocument(d.documentName))
    params.set("passageQuery", true)
    params.set("passageSize", 50)
    params.set("passageShift", 25)
    params.set("casefold", false)
    params.set("working", collectionRankings.map(_.name).toList.asJava) // !! from a first pass!

    // extract passages from the documents
    val passages = searcher.retrieveScoredDocuments(sdmQuery, Some(params), passageResults).map(passDoc => {
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
      (passDoc.score, passageBuilder.mkString)
    }).filter(x=>exampleRegex.pattern.matcher(x._2).matches())
    passages
  }

def getPassagesOntonotes(queryExample : String): Seq[(Double, String)] = //(Seq[parse.Document], Seq[String]) =
  {
    val sdmQuery = s"#combine(${GalagoQueryBuilder.seqdep(topic).queryStr} ${GalagoQueryBuilder.seqdep(queryExample).queryStr})"
    //    val sdmQuery = GalagoQueryBuilder.seqdep(exampleTerms.mkString(" ")).queryStr
    //    val exampleQuery = GalagoQueryLib.buildWeightedCombine(Seq((sdmQuery, 0.33), (GalagoQueryLib.buildWeightedCombine(exampleTerms), 1 - 0.33)))

    // initial document retrieval
    searcher.retrieveScoredDocuments(sdmQuery, Some(params), 111).map(d => (d.score, searcher.pullDocument(d.documentName).text)).filter(d=>exampleRegex.pattern.matcher(d._2).matches())

  }


  def labelData(passages : Seq[(Double, String)], bigramLabels :Boolean = false) : (Seq[Document], Seq[Document]) =
  {
    // Convert galago data to factorie docs
    val segmentPipeline = new DocumentAnnotationPipeline(
      Seq(DeterministicTokenizer, DeterministicSentenceSegmenter,
        OntonotesForwardPosTagger, BILOUChainChunker))
    val facDocs = rand.shuffle(passages.map(p => (p._2, new Document(p._2))))
    facDocs.foreach(d => segmentPipeline.process(d._2))

    // TODO this only handles bigrams - also not robust
    // label the tokens in the factorie docs
    facDocs.foreach(d => d._2.sentences.foreach(s => {
      s.tokens.sliding(2).foreach(bigram => {
        // if bigram is in example, use that
        if(!bigram(0).isSentenceEnd  && examples.contains(s"${bigram(0).string} ${bigram(1).string}")) {
          bigram(0).attr += new LabeledCustomNerTag(bigram(0), if (bigramLabels)"B-BRAN" else "U-BRAN")
          bigram(1).attr += new LabeledCustomNerTag(bigram(1), if (bigramLabels)"L-BRAN" else "U-BRAN")
        }
        // if not, is first token?
        else if (examples.contains(bigram(0).string))
          bigram(0).attr += new LabeledCustomNerTag(bigram(0), "U-BRAN")
        else
          bigram(0).attr += new LabeledCustomNerTag(bigram(0), "O")
        // dont forget the last token of sentence
        if (!bigram(0).isSentenceEnd && bigram(1).isSentenceEnd) {
          bigram(1).attr += new LabeledCustomNerTag(bigram(1), if (examples.contains(bigram(1).string)) "U-BRAN" else "O")
        }
      })
    }))

    // split into test, train
    val mid = (facDocs.size/3)*2
    val test = (facDocs take mid).map(_._2)
    val train = facDocs.takeRight(mid).map(_._2)
    (train, test)
  }

  def topK(model: ConllForwardNer, documents: Seq[Document]): Unit =
  {
    val uIndex = 3
    val scores = documents.flatMap(d => d.sentences.map(s =>
      new LightweightNerTokenSpan(s.tokens))
      .flatMap(s => s.tokens.filterNot(t => StopWords.containsWord(t.string))
      .flatMap(token => {
      val v = model.predictScores(token)
//      if (v.maxIndex == 0) // classified as no label
//        Seq()
//      else
//      if ((token.chunkTag.categoryValue.startsWith("B")||token.chunkTag.categoryValue.startsWith("I"))
//        && token.idxInSentence +1 < token.sentence.length){
//        val next = token.sentence(token.idxInSentence+1)
//        val v2 = model.predictScores(next)
//        Seq((token.string, v(uIndex)), (s"${token.string} ${next.string}", Math.max(v(uIndex),v2(uIndex))) )//(v(4)+v2(4))/2.0)) //
//      }
//      else // unigram
       val maxScore = Seq (v(1), v(2), v(3)).zipWithIndex.maxBy(_._1)
       val tString = if (maxScore._2 == 0 && token.idxInSentence < token.sentence.length -1)
         token.string + " " + token.sentence(token.idxInSentence +1 ).string else token.string

       Seq( (tString,  maxScore._1, maxScore._2))
    }))).sortBy(-_._2)

//    scores.filterNot(x => examples.contains(x._1)).take(50).foreach(println)
    scores.filterNot(x => examples.contains(x._1)).groupBy(_._1).map(_._2.maxBy(_._2)).toSeq.sortBy(-_._2).take(50).foreach(println)


  }

}
