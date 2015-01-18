/**
 * Created by pv on 1/16/15.
 */

import java.io.{PrintWriter, File}

import cc.factorie.app.nlp.{Document, _}
import cc.factorie.app.nlp.ner._
import cc.factorie.app.nlp.parse.ParseTree
import cc.factorie.app.nlp.pos.PennPosTag

import scala.collection.mutable.ArrayBuffer
import scala.io.Source


object Ontonoting  extends App{

  val docs = loadDir()
  exportToTrec()

  def exportToTrec() {
    // export to trec form
    val writer = new PrintWriter("trec-data-test")
    try {
      docs.foreach(doc => {
        writer.println(
          s"<DOC>\n" +
            s"<DOCNO>${doc.name}</DOCNO>\n" +
            "<TEXT>\n" +
            s"${doc.string}" +
            "</TEXT>\n" +
            "</DOC>"
        )
      })
    } catch {
      case e: Exception => println(e)
    }
    finally {
      writer.close()
    }
  }

  def loadDir(): Seq[Document] ={
    // read in data
    val dir = new File("data/trn-nament/")
    dir.listFiles().flatMap(f => LoadOntonotes5.fromFilename(f.toString))
  }
}



object AnnotationTypes{
  val NONE = 0
  val GOLD = 1
  val AUTO = 2
}

object LoadOntonotes5 {
  private def addDepInfo(s: Sentence, depInfoSeq: Seq[(Int,Int,String)]): Unit = {
    //assert(depInfoSeq.map(_._1) == Seq.tabulate(depInfoSeq.length)(i => i), "Token indices: "+depInfoSeq.map(_._1).mkString(" ")) // Assert that we have an entry for each token index, in order
    val tree = new ParseTree(s, depInfoSeq.map(_._2), depInfoSeq.map(_._3))
    s.attr += tree
  }

  def fromLines(lines:Iterator[String], fileName:String = "?UNKNOWN?", loadLemma:Int = AnnotationTypes.GOLD,
                loadPos:Int = AnnotationTypes.GOLD, loadParse:Int = AnnotationTypes.GOLD, loadNer:Boolean = true, nerBilou:Boolean = true):
  Seq[Document] = {
    val newDocResult: (Document, Sentence) = newDoc(fileName, 0)
    var document: Document = newDocResult._1
    var sentence: Sentence = newDocResult._2
    val docs = new ArrayBuffer[Document]
    var depInfoSeq = new collection.mutable.ArrayBuffer[(Int,Int,String)]
    var i = 1
    for (line <- lines) {
      if (line.length < 2) { // Sentence boundary
        document.appendString("\n")
        addDepInfo(sentence, depInfoSeq)
        depInfoSeq = new collection.mutable.ArrayBuffer[(Int,Int,String)]
        sentence = null
      } else {

        if (sentence eq null) {
          docs.append(document)
          val result = newDoc(fileName, i)
          document = result._1
          sentence = result._2
          i+= 1
        }

        val fields = line.split('\t')
//        assert(fields.length >= 10, "Fewer than 10 fields in file "+filename+"\nOffending line:\n"+line)

        val currTokenIdx = fields(0).toInt - 1
        val word = fields(1)

        var ner = fields(7); if (ner == "_") ner = "O"  // If we wanted to distinguish "unnamed entities" from background, we wouldn't have this.

        document.appendString(" ")

        val token = new Token(sentence, word)

        if (loadNer) token.attr += (if (nerBilou) new LabeledBilouOntonotesNerTag(token, ner) else new LabeledBioOntonotesNerTag(token, ner))
      }
    }
    if ((sentence != null) && (loadParse != AnnotationTypes.NONE)) addDepInfo(sentence, depInfoSeq)
    if (nerBilou) convertBioBilou(document.asSection)

    println("Loaded " + docs.size + "  document with "+document.sentences.size+" sentences with "+document.asSection.length+" tokens total from file "+fileName)
//    val docs = document.sentences.map(new Document(_))
    docs.toSeq
  }

  def newDoc(fileName :String, sentenceNumber : Int): (Document, Sentence) = {
    val document: Document = new Document().setName(fileName + "_" + sentenceNumber)
    document.annotators(classOf[Token]) = UnknownDocumentAnnotator.getClass // register that we have token boundaries
    document.annotators(classOf[Sentence]) = UnknownDocumentAnnotator.getClass // register that we have sentence boundaries
    document.annotators(classOf[PennPosTag]) = UnknownDocumentAnnotator.getClass // register that we have POS tags
    document.annotators(classOf[BilouOntonotesNerTag]) = UnknownDocumentAnnotator.getClass
    val sentence: Sentence = new Sentence(document)
    (document, sentence)
  }

  def fromFilename(filename:String, loadLemma:Int = AnnotationTypes.GOLD, loadPos:Int = AnnotationTypes.GOLD, loadParse:Int = AnnotationTypes.GOLD, loadNer:Boolean = true, nerBilou:Boolean = true): Seq[Document] = {
    fromLines(Source.fromFile(filename).getLines(), filename, loadLemma, loadPos, loadParse, loadNer, nerBilou)
  }

  // TODO Don't we need convertIobBilou since CoNLL 2003 data is actually in IOB format? -akm
  def convertBioBilou(section:Section): Unit = {
    /** Return the string of the NER label, including the two letter (B- or I-) prefix. */
    def cat(token:Token): String = if (token eq null) "null" else token.attr[BilouOntonotesNerTag].categoryValue
    /** Return true if the strings are equal without their two letter (B- or I-) prefix. */
    def sim(s1:String, s2:String): Boolean = s1.drop(2) == s2.drop(2)
    def isU(cat1:String, cat2:String, cat3:String): Boolean = cat2(0) == 'B' && (!sim(cat2, cat3) || cat3(0) == 'B')
    def isB(cat1:String, cat2:String, cat3:String): Boolean = cat2(0) == 'B' && sim(cat2, cat3) && cat3(0) == 'I'
    def isL(cat1:String, cat2:String, cat3:String): Boolean = cat2(0) == 'I' && sim(cat1, cat2) && (cat3(0) == 'B' || !sim(cat2, cat3))
    def isI(cat1:String, cat2:String, cat3:String): Boolean = cat2(0) == 'I' && cat3(0) == 'I'
    for (token <- section.tokens) if (token.attr[LabeledBilouOntonotesNerTag].intValue != 0) {
      val nerLabel = token.attr[LabeledBilouOntonotesNerTag]
      val cat1 = cat(token.prev); val cat2 = cat(token); val cat3 = cat(token.next)
      if (isU(cat1, cat2, cat3)) nerLabel.target.setCategory("U-"+cat2.drop(2))(null)
      else if (isB(cat1, cat2, cat3)) nerLabel.target.setCategory("B-"+cat2.drop(2))(null)
      else if (isL(cat1, cat2, cat3)) nerLabel.target.setCategory("L-"+cat2.drop(2))(null)
      else if (isI(cat1, cat2, cat3)) nerLabel.target.setCategory("I-"+cat2.drop(2))(null)
      nerLabel.setToTarget(null)
    }
  }

  def printDocument(d: Document) =
    for (s <- d.sentences)
      println(s.attr[ParseTree].toString() + "\n")

}