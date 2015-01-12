import cc.factorie.app.nlp.{segment, DocumentAnnotationPipeline, Document}
import edu.umass.ciir.strepsimur.galago.{GalagoParamTools, GalagoQueryBuilder, GalagoQueryLib, GalagoSearcher}
import org.lemurproject.galago.core.retrieval.ScoredDocument

import org.lemurproject.galago.utility.Parameters

import scala.collection.JavaConverters._

/**
 * Created by pv on 1/12/15.
 */
object Main extends App {

  // set up galago
  val skipIndex = Seq(8, 28, 37, 38)
  val bookIndex = "/mnt/nfs/indexes/ClueWeb12/galago/clueweb-12-B13.index/"
  var p = Params.clueb

  val indexParam = new Parameters()
  indexParam.set("index", bookIndex)

  val searcher = GalagoSearcher(indexParam)

//  val topic = "progressive rock bands"
//  val examples = Seq("Rush", "Yes", "Genesis", "Emerson Lake & Palmer", "King Crimson")
  val topic = "former presidents of the united states"
  val examples = Seq("George Washington", "Bill Clinton", "Harry Truman")
  val exampleRegex = examples.mkString("(", ")|(", ")")
  println(exampleRegex)
  val exampleTerms = examples.map((_,1.0)) //.mkString("#combine((", ")#combine( ", "))")

  val sdmQuery = GalagoQueryBuilder.seqdep(topic).queryStr
  val exampleQuery = GalagoQueryLib.buildWeightedCombine(Seq((sdmQuery, 0.55), (GalagoQueryLib.buildWeightedCombine(exampleTerms), 1 - 0.55)))

  val collectionRanking = searcher.retrieveScoredDocuments(sdmQuery, Some(p), 50).map(d=> searcher.pullDocument(d.documentName))

  val segmentPipeline = new DocumentAnnotationPipeline(Seq(segment.DeterministicTokenizer, segment.DeterministicSentenceSegmenter))
  val segmentedDocs = collectionRanking.map(doc => new Document(doc.text)).map(segmentPipeline.process)
  val sentences = segmentedDocs.flatMap(doc => doc.sentences)//.filter(_.string.matches(exampleRegex)))

  sentences.foreach(s => println(s.string))



}
