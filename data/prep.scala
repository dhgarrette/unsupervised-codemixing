import nlp.hmm.prob.SimpleExpProbabilityDistribution

val devaChars = ((0 until 128).map(i => "\\u0" + Integer.toHexString(0x0900 + i)) ++ Vector("।")).mkString
val DevaRe = raw"[\d\p{Punct}${devaChars}]*[${devaChars}][\d\p{Punct}${devaChars}]*".r
val LatnRe = raw"[\d\p{Punct}A-za-z]*[A-za-z][\d\p{Punct}A-za-z]*".r
val NumRe = raw"\d+".r
val datadir = "/Users/dhgarrette/workspace/unsupervised-codemixing/data"
val ConstantTypes = Vector("<UNK>", "<S>", "</S>", "<PAD>")

def removePunc(s: String) = s.toLowerCase.replaceAll(raw"[\d\p{Punct}।]", "")





writeUsing(File(s"$datadir/latn_wordlist.txt")) { f =>
  val embWords: Vector[String] =
    for {
      fn <- Vector(
          s"$datadir/emb/polyglot/en.emb",
          s"$datadir/emb/polyglot-shared/en.emb"
      )
      line <- File(fn).readLines
      word = line.splitWhitespace.apply(0)
      if LatnRe.matches(word)
    } yield word
  val cmWords: Vector[String] =
    for {
      fn <- Vector(
          s"$datadir/fire/en_hi/dev.langid.spl",
          s"$datadir/fire/en_hi/test.langid.spl",
          s"$datadir/icon/en_hi/dev.langid.spl",
          s"$datadir/icon/en_hi/test.langid.spl",
          s"$datadir/irshad/en_hi/dev.langid.spl",
          s"$datadir/irshad/en_hi/test.langid.spl",
          s"$datadir/irshad/en_hi/train.langid.spl",
          s"$datadir/irshad/en_hi_normalized_oracle/dev.langid.spl",
          s"$datadir/irshad/en_hi_normalized_oracle/test.langid.spl",
          s"$datadir/irshad/en_hi_normalized_oracle/train.langid.spl",
          s"$datadir/msr/en_hi/dev.langid.spl",
          s"$datadir/msr/en_hi/test.langid.spl",
      )
      line <- File(fn).readLines
      item <- line.splitWhitespace
      Vector(word, lang) = item.rsplit("\\|", 2)
      if LatnRe.matches(word)
    } yield word
  val udWords: Vector[String] =
    for {
      dataset <- Vector("train", "dev", "test")
      fn <- Vector(
          s"$datadir/ud/UD_English-EWT-master/$dataset.conllu",
      )
      line <- File(fn).readLines
      if line.nonEmpty
      if !line.startsWith("#")
      word = line.splitWhitespace.apply(1)
      if LatnRe.matches(word)
    } yield word
  (embWords ++ cmWords ++ udWords).flatMap(w => Vector(w, w.toLowerCase, removePunc(w))).filter(_.nonEmpty).sorted.distinct.foreach(f.writeLine)
}

writeUsing(File(s"$datadir/deva_wordlist.txt")) { f =>
  val embWords: Vector[String] =
    for {
      fn <- Vector(
          s"$datadir/emb/polyglot/hi.emb",
          s"$datadir/emb/polyglot-shared/hi.emb",
      )
      line <- File(fn).readLines
      word = line.splitWhitespace.apply(0)
      if DevaRe.matches(word)
    } yield word
  val cmWords: Vector[String] =
    for {
      fn <- Vector(
          s"$datadir/irshad/en_hi_normalized_oracle/dev.langid.spl",
          s"$datadir/irshad/en_hi_normalized_oracle/test.langid.spl",
          s"$datadir/irshad/en_hi_normalized_oracle/train.langid.spl",
      )
      line <- File(fn).readLines
      item <- line.splitWhitespace
      Vector(word, lang) = item.rsplit("\\|", 2)
      if DevaRe.matches(word)
    } yield word
  val udWords: Vector[String] =
    for {
      dataset <- Vector("train", "dev", "test")
      fn <- Vector(
          s"$datadir/ud/UD_Hindi-HDTB-master/$dataset.conllu",
      )
      line <- File(fn).readLines
      if line.nonEmpty
      if !line.startsWith("#")
      word = line.splitWhitespace.apply(1)
      if DevaRe.matches(word)
    } yield word
  (embWords ++ cmWords ++ udWords).flatMap(w => Vector(w, w.toLowerCase, removePunc(w))).filter(_.nonEmpty).sorted.distinct.foreach(f.writeLine)
}

val deva2latnTranslitDists =
  (File(s"$datadir/translit/deva2latn.txt").readLines ++ Vector("। .:1") ++ ConstantTypes.map(w => s"$w $w|1")).flatMap { line =>
  	  val Vector(deva, translitsString) = line.lsplit("\\s+", 2)
  	  val translits = translitsString.lsplit(",")
                        .map(_.rsplit(":", 2).map(_.trim))
                        .collect { case Vector(word, prob) => (word, prob.toDouble) }
                        .toVector
    if (translits.nonEmpty)
      Some(deva -> ((translits, new SimpleExpProbabilityDistribution(translits.toMap))))
    else
      None
  }.toMap

val latn2devaTranslitDists =
  (File(s"$datadir/translit/latn2deva.txt").readLines ++ ConstantTypes.map(w => s"$w $w|1")).flatMap { line =>
  	  val Vector(deva, translitsString) = line.lsplit("\\s+", 2)
  	  val translits = translitsString.lsplit(",")
                        .map(_.lsplit(":").map(_.trim))
                        .collect { case Vector(word, prob) => (word, prob.toDouble) }
                        .toVector
    if (translits.nonEmpty)
      Some(deva -> ((translits, new SimpleExpProbabilityDistribution(translits.toMap))))
    else
      None
  }.toMap

val latnVocab: Set[String] = File(s"$datadir/translit/latn_vocab.txt").readLines.flatMap(w => Set(w, w.toLowerCase, removePunc(w))).toSet
  
for (embset <- Vector("polyglot", "polyglot-shared")) {
  val devaEmbeddings: Map[String, String] =
    File(s"$datadir/emb/$embset/hi.emb").readLines.map { line =>
      	line.lsplit("\\s+", 2).toTuple2
    }.toMap

  writeUsing(File(s"$datadir/emb/$embset/hi_latn.emb")) { f =>
    val variants: Iterator[(String, (String, Double))] =  // latn -> {dist over embeddings from deva words that can be transliterated to the latn word} 
      devaEmbeddings.iterator.flatMap { case (devaWord, emb) =>
        devaWord match {
        	  case _ if ConstantTypes.contains(devaWord) =>
        	    Set(devaWord -> (emb, 100.0))
          case LatnRe() =>
            //if (latnVocab(devaWord))
              Set(devaWord -> (emb, 100.0))
            //else
            //   Set.empty[(String, (String, Double))]
          case _ =>
            deva2latnTranslitDists.get(devaWord).map { case (ts, d) =>
              ts.flatCollect { case (v, p) => //if latnVocab.contains(v) =>
                // Do a weighted coin flip for each weighted variant produced by the transliteration model.
                new SimpleExpProbabilityDistribution(Map(Some(v -> (emb, p)) -> p, None -> (100-p))).sample()
              }
            }.getOrElse(Set.empty)
        }
      }
    val variantEmbSamplers: Map[String, SimpleExpProbabilityDistribution[String]] =  // latn -> {dist over embeddings from deva words that can be transliterated to the latn word} 
        variants.groupByKey.mapVals(variantEmbs => new SimpleExpProbabilityDistribution(variantEmbs.toMap))
    for (latnWord <- ConstantTypes) {
      variantEmbSamplers.get(latnWord).foreach { d =>
    	    f.writeLine(s"$latnWord ${d.sample()}")
      }
    }
    for ((latnWord, embDist) <- variantEmbSamplers) {
      if (!ConstantTypes.contains(latnWord)) {
    	    f.writeLine(s"$latnWord ${embDist.sample()}")
      }
    }
  }

  writeUsing(File(s"$datadir/emb/$embset/en-hi_latn.emb")) { f => 
    val en = File(s"$datadir/emb/$embset/en.emb").readLines.map(_.lsplit("\\s+",2).toTuple2).toMap
    val hi = File(s"$datadir/emb/$embset/hi_latn.emb").readLines.map(_.lsplit("\\s+",2).toTuple2).toMap
    val averagedConstLines = ConstantTypes.mapTo(w => (en(w).splitWhitespace zipSafe hi(w).splitWhitespace)
                                                          .map { case (ee, he) => f"${(ee.toDouble + he.toDouble) / 2}%.7f" }
                                                          .mkString(" "))
    (averagedConstLines ++ ((en -- ConstantTypes).toVector ++ (hi -- ConstantTypes)).shuffled).map{case (w,e) => s"$w $e"}.foreach(f.writeLine)
  }
}

val OkayRe = raw"[\d\p{Punct}]+".r
for (set <- Vector("train", "dev", "test")) {
  writeUsing(File(s"$datadir/ud/UD_Hindi-HDTB-master_Latn/$set.conllu")) { f =>
    for (line <- File(s"$datadir/ud/UD_Hindi-HDTB-master/$set.conllu").readLines) {
      if (line.startsWith("#")) f.writeLine(line)
      else if (line.trim().isEmpty) f.writeLine(line)
      else {
        val splitLine = line.lsplit("\t", 4)
        val originalWord = splitLine(1)
        val variant = originalWord match {
          case OkayRe() => originalWord 
          case _ => deva2latnTranslitDists.get(originalWord).map(_._2.sample()).getOrElse{println("no translit: "+originalWord); "<UNK>"}
        }
        f.writeLine(s"${splitLine(0)}\t$variant\t$variant\t${splitLine.drop(3).mkString("\t")}|Original=$originalWord")
      }
    }
  }
}

writeUsing(File(s"$datadir/ud/en_hi/train.conllu")) { f => 
  val en = File(s"$datadir/ud/UD_English-EWT-master/train.conllu").readLines.splitWhere(_.isEmpty).toVector
  val hi = File(s"$datadir/ud/UD_Hindi-HDTB-master/train.conllu").readLines.splitWhere(_.isEmpty).toVector
  (en ++ hi).shuffled.foreach(s => f.writeLine(s.mkString("\n")+"\n"))
}

writeUsing(File(s"$datadir/ud/en-hi_latn/train.conllu")) { f => 
  val en = File(s"$datadir/ud/UD_English-EWT-master/train.conllu").readLines.splitWhere(_.isEmpty).toVector
  val hi = File(s"$datadir/ud/UD_Hindi-HDTB-master_Latn/train.conllu").readLines.splitWhere(_.isEmpty).toVector
  (en ++ hi).shuffled.foreach(s => f.writeLine(s.mkString("\n")+"\n"))
}

for (langdir <- Vector("UD_English-EWT-master", "UD_Hindi-HDTB-master_Latn")) {
  writeUsing(File(s"$datadir/ud/$langdir/train_latn.spl")) { f =>
    Vector(s"$datadir/ud/$langdir/train.conllu").foreach { fn =>
      println(fn)
      File(fn).readLines.filterNot(_.startsWith("#")).splitWhere(_.trim.isEmpty).foreach { sentenceLines => 
        f.writeLine(sentenceLines.map(_.splitWhitespace.apply(1).toLowerCase).mkString(" "))
      }
    }
  }
}
