    val LatnRe = raw"[\d\p{Punct}A-za-z]+".r
    val datadir = "/Users/dhgarrette/workspace/unsupervised-codemixing/data"

    for (evalset <- Vector("dev", "test")) {
      writeUsing(File(s"$datadir/irshad/en_hi/${evalset}.conllu")) { outputFile =>
      writeUsing(File(s"$datadir/irshad/en_hi_normalized_oracle/${evalset}.conllu")) { normOutputFile =>
        val conlluSentences = File(s"$datadir/irshad/en_hi/TWEETS-${evalset}-v2_utf.conllu").readLines.filterNot(_.startsWith("#")).splitWhere(_.isEmpty).filter(_.nonEmpty)
        val kelseySentences = File(s"$datadir/irshad/en_hi/original-${evalset}.conllu").readLines.filterNot(_.startsWith("#")).splitWhere(_.isEmpty).filter(_.nonEmpty)
        for ((conlluSentenceLines, kelseySentenceLines) <- (conlluSentences zipSafe kelseySentences)) {
          assert(conlluSentenceLines.size == kelseySentenceLines.size,
                 s"conlluSentenceLines.size=${conlluSentenceLines.size} != kelseySentenceLines.size=${kelseySentenceLines.size}\n${kelseySentenceLines.mkString("\n")}\n${kelseySentenceLines.mkString("\n")}")
          for ((conlluLine, kelseyLine) <- (conlluSentenceLines zipSafe kelseySentenceLines)) {
            val Vector(ci, cnormWordOrGarbage, corigWord, cpos, _, _, chead, cdep, clangAndBio, _) = conlluLine.splitWhitespace
            val Vector(ki, korigWord, knormWordOrGarbage, kpos, _, _, khead, kdep, klangTag, _) = kelseyLine.splitWhitespace

            val Vector(clangTag, clangCode) = clangAndBio.lsplit("\\|(B|I)-")
            val clang = (klangTag, clangCode) match { case ("en",_) => "en"; case ("hi",_) => "hi"; case (_,"E") => "en"; case (_,"H") => "hi" }
            //val coutputWord = clang match { case "en" => corigWord; case "hi" => cnormWordOrGarbage }
            val knormOutputWord = knormWordOrGarbage match { case LatnRe() => korigWord; case _ => knormWordOrGarbage }
            
            assert(ci == ki, s"ci != ki\n$conlluLine\n$kelseyLine")
            assert(corigWord == korigWord, s"corigWord != korigWord\n$conlluLine\n$kelseyLine")

            outputFile.writeLine(s"$ki\t$korigWord\t$korigWord\t$kpos\t_\t_\t$khead\t$kdep\t$clangTag\tLang=$clang")
            normOutputFile.writeLine(s"$ki\t$knormOutputWord\t$knormOutputWord\t$kpos\t_\t_\t$khead\t$kdep\t$clangTag\tLang=$clang")
          }
          outputFile.writeLine()
          normOutputFile.writeLine()
        }
      }
      }
    }

    for (dataset <- Vector("en_hi", "en_hi_normalized_oracle")) {
      for (evalset <- Vector("dev", "test")) {
        writeUsing(File(s"$datadir/irshad/$dataset/$evalset.langid.spl")) { w =>
          for (sentenceLines <- File(s"$datadir/irshad/$dataset/${evalset}.conllu").readLines.splitWhere(_.isEmpty).filter(_.nonEmpty)) {
            w.writeLine(sentenceLines.map(_.splitWhitespace)
                                     .map { case cols => s"${cols(1)}|${cols.last.lsplit(",").map(_.rsplit("=").toTuple2).toMap.apply("Lang")}" }
                                     .mkString(" "))
          }
        }
      }
    }
