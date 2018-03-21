package nlp.hmm.data

import dhg.util._
import scala.collection.mutable.Buffer

object Tsv2Spl {
 def main(args: Array[String]) = {
   
   val lang1 = "en"
   val lang2 = "es"
   
   for (filename <- Vector(
       "/Users/dhgarrette/workspace/unsupervised-codemixing/data/wcacs16/en_es/train_data.tsv",
       "/Users/dhgarrette/workspace/unsupervised-codemixing/data/wcacs16/en_es/test_data.tsv",
       "/Users/dhgarrette/workspace/unsupervised-codemixing/data/wcacs16/en_es/dev_data.tsv",
       )) {
     writeUsing(File(filename.dropRight(3) + "spl")) { f =>
       val buffer = Buffer[String]()
       var prevTweetId = "0"
       for (line <- File(filename).readLines) {
         val Vector(tweetId, userId, start, end, word, lang) = line.splitWhitespace match {
           case v if v.size == 6 => v
           case v if v.size == 5 => v ++ Vector("???")
         }
         
         if (tweetId != prevTweetId && buffer.nonEmpty) {
           f.writeLine(buffer.mkString(" "))
           buffer.clear()
         }
         
         val langName = lang match {
           case "lang1" => lang1
           case "lang2" => lang2
           case other => other
         }
         buffer += s"$word|$langName"
      		 prevTweetId = tweetId
       }
       if (buffer.nonEmpty) {
         f.writeLine(buffer.mkString(" "))
         buffer.clear()
       }
     }   
   }
 }
}
