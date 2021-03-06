import cc.factorie.app.nlp.embeddings.EmbeddingOpts
import cc.factorie.app.nlp.lexicon.StopWords
import cc.factorie.app.nlp.load.BILOUChunkTag
import cc.factorie.app.nlp.phrase.{PhraseList, NPChunkMentionFinder, BILOUChainChunker}
import cc.factorie.app.nlp.pos.{PennPosTag, OntonotesForwardPosTagger}
import cc.factorie.app.nlp.segment.{DeterministicSentenceSegmenter, DeterministicTokenizer}
import cc.factorie.app.nlp.{Document, DocumentAnnotationPipeline}
import cc.factorie.util.DefaultCmdOptions
import co.pemma.DocReader
import co.pemma.embeddings.UpdateableSkipGramEmbeddingModel
import edu.umass.ciir.strepsimur.galago.{GalagoQueryBuilder, GalagoSearcher}
import edu.umass.cs.iesl.lffi.ner._
import org.lemurproject.galago.core.retrieval.ScoredPassage
import org.lemurproject.galago.utility.Parameters

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.util.matching.Regex

/**
 * Created by pv on 1/12/15.
 */

object Main {
  val rand = new Random(69)


  def main(args: Array[String]) {

    val opts = new ActiveLearnOpts
    opts.parse(args)

    val (topic, examples) = opts.topic.value match {
      case "dogs" => ("small breeds of dogs",
        Seq("chihuahua", "dachshund", "pug", "terrier", "beagle"))
      case "datasets" => ("data sets in information retrieval",
        Seq("robust04", "clueweb", "clueweb09"))
      case "states" => ("states in the United States of America",
        Seq("Massachusetts", "Montana", "Florida",
          "South Carolina", "New York", "North Dakota"))
      case "fruits" => ("types of fruits",
        Seq("apple", "orange", "banana", "lime", "kiwi"))
      case "politicians" => ("United States of America politicians",
        Seq("Bill Clinton", "John Kerry", "George Bush", "Ted Kennedy", "John Edwards", "Joe Lieberman"))
    }

    val exampleRegex = examples.mkString(".*((", ")|(", ")).*").r
    println(s"$topic : ${examples.mkString(", ")}")

    val segmentPipeline = new DocumentAnnotationPipeline(
      Seq(DeterministicTokenizer, DeterministicSentenceSegmenter,
        OntonotesForwardPosTagger, BILOUChainChunker, NPChunkMentionFinder))

    val (searcher, params) = initializeGalago(opts.corpus.value)

    // get initial collection documents
    val collectionRankings = examples.flatMap(queryExample => {searcher.retrieveScoredDocuments(
      s"#combine(${GalagoQueryBuilder.seqdep(topic).queryStr} ${GalagoQueryBuilder.seqdep(queryExample).queryStr})", Some(params), opts.docResults.value)
      .map(d => searcher.pullDocument(d.documentName))}).toSet.toSeq

     collectionRankings.map(d => DocReader.parseRobust(d.text)).mkString("\n").split("\n").filterNot(_.isEmpty).foreach(println)

//    // set params and retrieve passages
//    params.set("passageQuery", true)
//    params.set("passageSize", 50)
//    params.set("passageShift", 25)
//    params.set("casefold", false)
//    params.set("working", collectionRankings.map(_.name).toList.asJava) // !! from a first pass!
//    val passages = examples.flatMap(getPassages(_, exampleRegex, topic, opts.passageResults.value, collectionRankings, searcher, params))
//    assert(passages.size > 0, "no passages found")

    // Convert galago data to factorie docs

//    val facDocs = rand.shuffle(passages.map(p => new Document(p._2)))
//    facDocs.foreach(d => segmentPipeline.process(d))
//    embeddings(facDocs, examples)

    //    val (train, test) = labelData(facDocs, examples, bigramLabels = true)


    // split into test, train
//    val mid = (facDocs.size / 3) * 2
//    val test = facDocs take mid
//    val train = facDocs.takeRight(mid)
    //    val model = new ConllForwardNer
    //    model.train(train, test, iters=3, negSamplesPerPos=opts.negativeSamples.value)(rand)
    //    topK(model, test, examples)


    //  val model = new CustomNerChainCRF()
    //  model.train(train++ test.take(test.size-1), Seq(test.last))(rand)
  }


  def initializeGalago(corpus: String): (GalagoSearcher, Parameters) = {
    val (index, params) = corpus match {
      case "clueweb" => ("/mnt/nfs/indexes/ClueWeb12/galago/clueweb-12-B13.index", Params.clueb)
      case "robust" => ("./index/robust04", Params.robustTitleDoc)
    }

    val indexParam = new Parameters()
    indexParam.set("index", index)
    (GalagoSearcher(indexParam), params)
  }


  def getPassages(queryExample: String, exampleRegex: Regex, topic: String, passageResults :Int,
                  collectionRankings : Seq[org.lemurproject.galago.core.parse.Document], searcher: GalagoSearcher, params: Parameters):
  Seq[(Double, String)] =
  {
    // extract passages from the documents
    val passages = searcher.retrieveScoredDocuments(
      s"#combine(${GalagoQueryBuilder.seqdep(topic).queryStr} ${GalagoQueryBuilder.seqdep(queryExample).queryStr})",
      Some(params), passageResults).map(passDoc =>
    {
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
    }).filter(x => exampleRegex.pattern.matcher(x._2).matches())

     passages
  }

  //def getPassagesOntonotes(queryExample : String): Seq[(Double, String)] = //(Seq[parse.Document], Seq[String]) =
  //  {
  //    val sdmQuery = s"#combine(${GalagoQueryBuilder.seqdep(topic).queryStr} ${GalagoQueryBuilder.seqdep(queryExample).queryStr})"
  //    //    val sdmQuery = GalagoQueryBuilder.seqdep(exampleTerms.mkString(" ")).queryStr
  //    //    val exampleQuery = GalagoQueryLib.buildWeightedCombine(Seq((sdmQuery, 0.33), (GalagoQueryLib.buildWeightedCombine(exampleTerms), 1 - 0.33)))
  //
  //    // initial document retrieval
  //    searcher.retrieveScoredDocuments(sdmQuery, Some(params), 111).map(d => (d.score, searcher.pullDocument(d.documentName).text)).filter(d=>exampleRegex.pattern.matcher(d._2).matches())
  //
  //  }


  def labelData(facDocs: Seq[Document], examples: Seq[String], bigramLabels: Boolean = false) =
  {
    // TODO this only handles bigrams - also not robust
    // label the tokens in the factorie docs
    facDocs.foreach(d => d.sentences.foreach(s => {
      s.tokens.sliding(2).foreach(bigram => {
        // if bigram is in example, use that
        if (!bigram(0).isSentenceEnd && examples.contains(s"${bigram(0).string} ${bigram(1).string}")) {
          bigram(0).attr += new LabeledCustomNerTag(bigram(0), if (bigramLabels) "B-BRAN" else "U-BRAN")
          bigram(1).attr += new LabeledCustomNerTag(bigram(1), if (bigramLabels) "L-BRAN" else "U-BRAN")
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
  }

  def topK(model: ConllForwardNer, documents: Seq[Document], examples: Seq[String]): Unit = {
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
      val maxScore = Seq(v(1), v(2), v(3)).zipWithIndex.maxBy(_._1)
      val tString = if (maxScore._2 == 0 && token.idxInSentence < token.sentence.length - 1)
        token.string + " " + token.sentence(token.idxInSentence + 1).string
      else token.string

      Seq((tString, maxScore._1, maxScore._2))
    }))).sortBy(-_._2)

    //    scores.filterNot(x => examples.contains(x._1)).take(50).foreach(println)
    scores.filterNot(x => examples.contains(x._1)).groupBy(_._1).map(_._2.maxBy(_._2)).toSeq.sortBy(-_._2).take(50).foreach(println)
  }

  def embeddings(documents: Seq[Document], examples: Seq[String]): Unit = {
    val opts = new EmbeddingOpts()
    opts.parse(Seq(
      "--ignore-stopwords=true",
      "--threads=48",
      "--encoding=UTF8",
      "--save-vocab=./vocab",
      "--min-count=1",
      //    "--load-vocab=./vocab",
      s"--train=./",
      s"--output=./"))

    val wordEmbedModel = new UpdateableSkipGramEmbeddingModel(opts)
        val phrases : Seq[String] = documents.flatMap(makePhrases)
//    val phrases = documents.flatMap(_.sentences.map(_.string)).toSeq
    wordEmbedModel.initializeFromStrings(phrases)
    wordEmbedModel.updateModel(phrases)

    examples.foreach(e => {
      println(e)
      wordEmbedModel.distance(e, 10, phrases)
    })
  }

  def makePhrases(facDoc: Document): Seq[String] = {
    //    val phrases = new ArrayBuffer[String]
    //    facDoc.asSection.sentences.foreach(s => {
    //      val tokens = s.tokens
    //      var i = 0
    //      while (i < tokens.size -1){
    //        var current = tokens(i)
    //        var phrase = current.string
    //        if (current.attr[BILOUChunkTag].value.toString().startsWith("B")) {
    //          while (current.next.attr[BILOUChunkTag].value.toString().startsWith("I")
    //            || current.next.attr[BILOUChunkTag].value.toString().startsWith("L")) {
    //            phrase += "_"+current.next.string
    //            current = current.next
    //            i += 1
    //          }
    //        }
    //        else i += 1
    //        phrases += phrase
    //      }
    //    })
    //    phrases.toSeq


    //    val usedTokens = scala.collection.mutable.Set[Int]()
    //    val docString = for (phrase <- facDoc.attr[PhraseList])
    //    yield {
    //      phrase.tokens.foreach(usedTokens += _.position)
    //      phrase.string.replaceAll("\\s+", "_")
    //    }
    //
    //    // collect phrases and tokens not in phrases
    //    docString ++ (for (token <- facDoc.tokens; str = token.string
    //                       if !usedTokens.contains(token.position) &&
    //                         !StopWords.containsWord(str.toLowerCase) &&
    //                         str.size > 1) yield token.string)
    //  }

    val phrases = new ArrayBuffer[String]
    facDoc.sentences.foreach(s => {
      val tokens = s.tokens
      var i = 0
      var str = ""
      while (i < tokens.size) {
        if (tokens(i).attr[PennPosTag].isNoun) {
          str = if (str == "") tokens(i).string else s"${str}_${tokens(i).string}"
        }
        else {
          if (str != "") phrases += str
          phrases += tokens(i).string
          str = ""
        }
        i += 1
      }
      if (str != "") phrases += str
    })
    phrases
  }
}


class ActiveLearnOpts extends DefaultCmdOptions
{
  val corpus = new CmdOption("corpus", "robust", "STRING", "which corpus/index to use")
  val topic = new CmdOption("topic", "states", "STRING", "Which topic set to use")
  val out = new CmdOption("out", "out.docs", "STRING", "Where to export corpus")
  val docResults = new CmdOption("docResults", 1000, "Int", "Number of doc results for each query")
  val passageResults = new CmdOption("passageResults", 250, "Int", "Number of top passages to extract from doc results")
  val negativeSamples = new CmdOption("negative", 3, "Int", "Number of negative samples to use for each positive sample")
}
