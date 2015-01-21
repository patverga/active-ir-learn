import java.io.PrintWriter

import co.pemma.DocReader
import edu.umass.ciir.strepsimur.galago.GalagoQueryBuilder

/**
 * Created by pv on 1/21/15.
 */
object ExportDocs extends App{

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

  val (searcher, params) = Main.initializeGalago(opts.corpus.value)

  // get initial collection documents
  val collectionRankings = examples.flatMap(queryExample => {searcher.retrieveScoredDocuments(
    s"#combine(${GalagoQueryBuilder.seqdep(topic).queryStr} ${GalagoQueryBuilder.seqdep(queryExample).queryStr})", Some(params), opts.docResults.value)
    .map(d => searcher.pullDocument(d.documentName))}).toSet.toSeq

  val writer = new PrintWriter(opts.out.value)
  try {
    val clean = collectionRankings.map(d => if (opts.corpus.value=="robust") DocReader.parseRobust(d.text) else d.text)
    clean.mkString("\n").split("\n").filterNot(_.isEmpty).foreach(writer.println)
  } catch {case e :Exception=> e.printStackTrace()}

}
