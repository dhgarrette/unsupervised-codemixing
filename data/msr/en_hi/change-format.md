    for (evalset <- Vector("dev", "test")) {
      writeUsing(File(s"/Users/dhgarrette/workspace/unsupervised-codemixing/data/msr/en_hi/$evalset.conllu")) { w =>
        for (line <- File(s"/Users/dhgarrette/workspace/unsupervised-codemixing/data/msr/en_hi/tagged-msr-hi_en-orig-coarse-$evalset.txt").readLines) {
          for ((Vector(word, lang, pos), i) <- line.splitWhitespace.map(_.rsplit("\\|")).zipWithIndex) {
            w.writeLine(s"$i\t$word\t$word\t$pos\t$pos\tLang=$lang")
          }
        }
      }
    }

    for (evalset <- Vector("dev", "test")) {
      writeUsing(File(s"/Users/dhgarrette/workspace/unsupervised-codemixing/data/msr/en_hi/$evalset.langid.spl")) { w =>
        for (line <- File(s"/Users/dhgarrette/workspace/unsupervised-codemixing/data/msr/en_hi/tagged-msr-hi_en-orig-coarse-$evalset.txt").readLines) {
          w.writeLine(line.splitWhitespace.map(_.rsplit("\\|")).map { case Vector(word, lang, pos) => s"$word|$lang" }.mkString(" "))
        }
      }
    }

