original word:          splitConlluLine[2]
token language ({E,H}): splitConlluLine[8].last 
normalized word:        lang match { case "E" => splitConlluLine[2]; case "H" => splitConlluLine[1] }


    val LatnRe = raw"[\d\p{Punct}A-za-z]+".r
    val datadir = "/Users/dhgarrette/workspace/unsupervised-codemixing/data"

    for (evalset <- Vector("dev", "test")) {
      writeUsing(File(s"$datadir/irshad/en_hi/${evalset}.conllu")) { w =>
        val conlluSentences = File(s"$datadir/irshad/en_hi/TWEETS-${evalset}-v2_utf.conllu").readLines.filterNot(_.startsWith("#")).splitWhere(_.isEmpty).filter(_.nonEmpty)
        val kelseySentences = File(s"$datadir/irshad/en_hi/original-${evalset}.conllu").readLines.filterNot(_.startsWith("#")).splitWhere(_.isEmpty).filter(_.nonEmpty)
        for ((conlluSentenceLines, kelseySentenceLines) <- (conlluSentences zipSafe kelseySentences)) {
          assert(conlluSentenceLines.size == kelseySentenceLines.size,
                 s"conlluSentenceLines.size=${conlluSentenceLines.size} != kelseySentenceLines.size=${kelseySentenceLines.size}\n${kelseySentenceLines.mkString("\n")}\n${kelseySentenceLines.mkString("\n")}")
          for ((conlluLine, kelseyLine) <- (conlluSentenceLines zipSafe kelseySentenceLines)) {
            val Vector(ci, cnormWordOrGarbage, corigWord, cpos, _, _, chead, cdep, clangAndBio, _) = conlluLine.splitWhitespace
            val Vector(clangTag, clangCode) = clangAndBio.lsplit("\\|(B|I)-")
            val clang = clangCode match { case "E" => "en"; case "H" => "hi" }
            //val coutputWord = clang match { case "en" => corigWord; case "hi" => cnormWordOrGarbage }

            val Vector(ki, korigWord, knormWordOrGarbage, kpos, _, _, khead, kdep, klangTag, _) = kelseyLine.splitWhitespace
            val koutputWord = korigWord match { case LatnRe() => korigWord; case _ => knormWordOrGarbage }
            
            assert(ci == ki, s"ci != ki\n$conlluLine\n$kelseyLine")
            assert(corigWord == korigWord, s"corigWord != korigWord\n$conlluLine\n$kelseyLine")

            w.writeLine(s"$ki\t$koutputWord\t$koutputWord\t$kpos\t$kpos\t_\t_\t$khead\t$kdep\t$clangTag\tLang=$clang")
          }
          w.writeLine()
        }
      }
    }

    for (evalset <- Vector("dev", "test")) {
      writeUsing(File(s"$datadir/irshad/en_hi/$evalset.langid.spl")) { w =>
        for (sentenceLines <- File(s"$datadir/irshad/en_hi/${evalset}.conllu").readLines.splitWhere(_.isEmpty).filter(_.nonEmpty)) {
          w.writeLine(sentenceLines.map(_.splitWhitespace).map { case cols => s"${cols(1)}|${cols.last.lsplit(",").map(_.rsplit("=").toTuple2).toMap.apply("Lang")}" }.mkString(" "))
        }
      }
    }
