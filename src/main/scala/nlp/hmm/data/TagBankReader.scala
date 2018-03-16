// package nlp.hmm.data

// import dhg.util._
// import scala.util.Try

// trait TagBankReader[Tag] {
//   type Word = String
//   def tdData(): Vector[Vector[(Word, Tag)]]
//   protected[this] def rawData: Vector[Vector[(Word, Tag)]]
//   final def raw(): Vector[Vector[Word]] = rawData.map(_.map(_._1))
//   final def rawCHEATING(): Vector[Vector[(Word, Tag)]] = rawData
//   def devData(): Vector[Vector[(Word, Tag)]]
//   def testData(): Vector[Vector[(Word, Tag)]]

//   def startWord: Word
//   def startTag: Tag
//   def endWord: Word
//   def endTag: Tag
//   def tagToString: (Tag => String)
//   def tagFromString: (String => Tag)
// }

// abstract class CcgTagBankReader[Tag] extends TagBankReader[Tag] {
//   protected[this] val Loc: String

//   protected[this] val TerminalRe = """<L ([\S]+) ([\S]+) ([\S]+) ([\S]+) ([\S]+)>""".r // <L (NP\NP)/NP IN IN of (NP_99\NP_99)/NP_100>
//   protected[this] val NonTerminalRe = """<T ([\S]+) ([\S]+) ([\S]+)>""".r
//   protected[this] val AtomicRe = """[^\\/]+""".r

//   /**
//    * section -> sentence -> token
//    */
//   protected[this] def doGetStrings(): Vector[Vector[Vector[(String, String, String)]]] = {
//     File(Loc).ls("\\d+".r).map { sectionDir =>
//       for (file <- sectionDir.ls; Seq(header, content) <- file.readLines.grouped(2)) yield {
//           assert(header.startsWith("ID="))
//           TerminalRe.allGroups(content).map {
//             case Seq(supertag, pos, _, token, _) =>
//               val cleanSupertag = supertag match {
//                 case "((S[b]\\NP)/NP)/" => "((S[b]\\NP)/NP)" // error in wsj_0595.15
//                 case "(S[dcl]\\NP)/(S[dcl]\\NP)~SB" => "((S[dcl]\\NP)/(S[dcl]\\NP))" // errors in chinese ccgbank
//                 case "." => """(S\S)"""
//                 case "," => """(NP\NP)"""
//                 case ";" => """((S\S)/S)"""
//                 case s @ AtomicRe() => s
//                 case s => s"($s)"
//               }
//               (token, cleanSupertag, pos)
//             case x => sys.error("Failure in reading CcgBank.  \nTerminal \"%s\"  \nfrom content line:%s\n".format(x, content))
//           }.toVector
//         }
//       }
//   }

//   protected[this] def data: Vector[Vector[Vector[(String, Tag)]]]

//   override final val startWord = "<S>"
//   override final val endWord = "<E>"
// }

// abstract class EnglishCcgTagBankReader[Tag] extends CcgTagBankReader[Tag] {
//   protected[this] val Loc = "data/ccgbank/AUTO"

//   override final def tdData = data.slyce(0 to 15).flatten
//   override final protected[this] def rawData = data.slyce(16 to 18).flatten
//   override final def devData = data.slyce(19 to 21).flatten
//   override final def testData = data.slyce(22 to 24).flatten
// }

// object PosEnglishCcgTagBankReader extends EnglishCcgTagBankReader[String] {
//   override /*protected[this]*/ val data = doGetStrings().map(_.map(_.mapt((w, s, p) => (w, p))))
//   override val startTag = "<S>"
//   override val endTag = "<E>"
//   override val tagToString: (String => String) = identity
//   override val tagFromString: (String => String) = identity
// }

// //
// //
// //

// object TestTagBankReader extends TagBankReader[String] {
//   val Loc = "data/test/AUTO"

//   val TokRe = """([^|\s]+)\|([^|\s]+)""".r

//   /**
//    * @return	section -> file -> sentence
//    */
//   val data = {
//     for {
//       section <- (0 to 24).toVector
//     } yield {
//       Option(File(Loc, f"$section%02d").listFiles).map { sectionDir =>
//         for {
//           file <- sectionDir.iterator
//           line <- file.readLines.map(_.trim)
//           if line.nonEmpty
//         } yield {
//           line.split("\\s+").map { case TokRe(w, t) => (w, t) }.toVector
//         }
//       }.fold(Vector[Vector[(String, String)]]())(_.toVector)
//     }
//   }

//   final def tdData = data.slyce(0 to 15).flatten
//   final protected[this] def rawData = data.slyce(16 to 18).flatten
//   final def devData = data.slyce(19 to 21).flatten
//   final def testData = data.slyce(22 to 24).flatten

//   override val startWord = "<S>"
//   override val startTag = "<S>"
//   override val endWord = "<E>"
//   override val endTag = "<E>"
//   override val tagToString: (String => String) = identity
//   override val tagFromString: (String => String) = identity
// }
