import java.io.PrintWriter
import java.util

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

  val writer = new PrintWriter(opts.out.value)
  try {
    // get initial collection documents
    val docSet = new util.HashSet[String]
    examples.foreach(queryExample => {
      searcher.retrieveScoredDocuments(
      s"#combine(${GalagoQueryBuilder.seqdep(topic).queryStr} ${GalagoQueryBuilder.seqdep(queryExample).queryStr})", Some(params), opts.docResults.value)
      .foreach(d => {
        if (!docSet.contains(d.documentName)) {
          docSet.add(d.documentName)
          val doc = searcher.pullDocument(d.documentName)
          val clean = if (opts.corpus.value=="robust") DocReader.parseRobust(doc.text) else doc.text
          clean.split("\n").filterNot(_.isEmpty).foreach(writer.println)
        }
      })
    })
  } catch {case e :Exception=> e.printStackTrace()}

}
