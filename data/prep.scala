import scalaz._
import Scalaz._
import nlp.hmm.prob.SimpleExpProbabilityDistribution

val devaChars = ((0 until 128).map(i => "\\u0" + Integer.toHexString(0x0900 + i)) ++ Vector("।")).mkString
val DevaRe = raw"[\d\p{Punct}${devaChars}]*[${devaChars}][\d\p{Punct}${devaChars}]*".r
val LatnRe = raw"[\d\p{Punct}A-za-z]*[A-za-z][\d\p{Punct}A-za-z]*".r
val NumRe = raw"\d+".r
val datadir = "/Users/dhgarrette/workspace/unsupervised-codemixing/data"
val ConstantTypes = Vector("<UNK>", "<S>", "</S>", "<PAD>")

def removePunc(s: String) = s.toLowerCase.replaceAll(raw"[\d\p{Punct}।]", "")


////////////////////////////////////////////////////////////////////////
// 1. IRSHAD STUFF
//  Inputs:
//      DATA/irshad/en_hi/TWEETS-{dev,test}-v2_utf.conllu     // Provided by Kelsey originally.
//      DATA/irshad/en_hi/original-{train,dev,test}.conllu    // Provided by Kelsey recently; 'train' treated as extra 'test' data.
//  Outputs:
//      DATA/irshad/en_hi/{dev,test}.conllu
//      DATA/irshad/en_hi/{dev,test}.langid.spl
//      DATA/irshad/en_hi_normalized_oracle/{dev,test}.conllu
//      DATA/irshad/en_hi_normalized_oracle/{dev,test}.langid.spl
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
        val lang = (klangTag, clangCode) match { case ("en",_) => "en"; case ("hi",_) => "hi"; case (_,"E") => "en"; case (_,"H") => "hi" }
        //val coutputWord = lang match { case "en" => corigWord; case "hi" => cnormWordOrGarbage }
        val knormOutputWord = knormWordOrGarbage match { case LatnRe() => korigWord; case _ => knormWordOrGarbage }
        
        assert(ci == ki, s"ci != ki\n$conlluLine\n$kelseyLine")
        assert(corigWord == korigWord, s"corigWord != korigWord\n$conlluLine\n$kelseyLine")

        outputFile.writeLine(s"$ki\t$korigWord\t$korigWord\t$kpos\t_\t_\t$khead\t$kdep\t$clangTag\tLang=$lang")
        normOutputFile.writeLine(s"$ki\t$knormOutputWord\t$knormOutputWord\t$kpos\t_\t_\t$khead\t$kdep\t$clangTag\tLang=$lang")
      }
      outputFile.writeLine()
      normOutputFile.writeLine()
    }
//    if (evalset == "test") {
//      val kelseySentences = File(s"$datadir/irshad/en_hi/original-train.conllu").readLines.filterNot(_.startsWith("#")).splitWhere(_.isEmpty).filter(_.nonEmpty)
//      for (kelseySentenceLines <- kelseySentences) {
//        for (kelseyLine <- kelseySentenceLines) {
//          val Vector(ki, korigWord, knormWordOrGarbage, kpos, _, _, khead, kdep, klangTag, _) = kelseyLine.splitWhitespace
//  
//          val knormOutputWord = knormWordOrGarbage match { case LatnRe() => korigWord; case _ => knormWordOrGarbage }
//          
//          outputFile.writeLine(s"$ki\t$korigWord\t$korigWord\t$kpos\t_\t_\t$khead\t$kdep\t_")
//          normOutputFile.writeLine(s"$ki\t$knormOutputWord\t$knormOutputWord\t$kpos\t_\t_\t$khead\t$kdep\t_")
//        }
//        outputFile.writeLine()
//        normOutputFile.writeLine()
//      }
//    }
  }
  }
  for (dataset <- Vector("en_hi", "en_hi_normalized_oracle")) {
    writeUsing(File(s"$datadir/irshad/$dataset/$evalset.langid.spl")) { w =>
      for (sentenceLines <- File(s"$datadir/irshad/$dataset/${evalset}.conllu").readLines.splitWhere(_.isEmpty).filter(_.nonEmpty)) {
        val outputTokens: Vector[Option[String]] =
            sentenceLines.map(_.splitWhitespace).map { cols =>
                Option(cols.last)
                    .filter(_ != "_")
                    .flatMap { _.lsplit(",")
                                .map(_.rsplit("=").toTuple2)
                                .toMap.get("Lang")
                    }
                    .map(lang => s"${cols(1)}|$lang")
            }
        if (outputTokens.forall(_.nonEmpty)) {
          w.writeLine(outputTokens.flatten.mkString(" "))
        }
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////
// 2. CM DATASET STUFF
//  Inputs:
//      DATA/{icon,msr}/en_hi/tagged-{icon,msr}-hi_en-orig-coarse-{dev,test}.txt
//  Outputs:
//      DATA/{icon,msr}/en_hi/{dev,test}.conllu
//      DATA/{icon,msr}/en_hi/{dev,test}.langid.spl
for (dataset <- Vector("icon", "msr")) {
for (evalset <- Vector("dev", "test")) {
  writeUsing(File(s"/Users/dhgarrette/workspace/unsupervised-codemixing/data/$dataset/en_hi/$evalset.conllu")) { conllOut =>
  writeUsing(File(s"/Users/dhgarrette/workspace/unsupervised-codemixing/data/$dataset/en_hi/$evalset.langid.spl")) { langidSplOut =>
    for (line <- File(s"/Users/dhgarrette/workspace/unsupervised-codemixing/data/$dataset/en_hi/tagged-$dataset-hi_en-orig-coarse-$evalset.txt").readLines) {
      for ((Vector(word, lang, pos), i) <- line.splitWhitespace.map(_.rsplit("\\|")).zipWithIndex) {
        conllOut.writeLine(s"$i\t$word\t$word\t$pos\t$pos\tLang=$lang")
      }
      conllOut.writeLine()
      langidSplOut.writeLine(line.splitWhitespace.map(_.rsplit("\\|")).map { case Vector(word, lang, pos) => s"$word|$lang" }.mkString(" "))
    }
  }
  }
}
}
////////////////////////////////////////////////////////////////////////
// 3.
//  Inputs:
//      DATA/fire/en_hi/{dev,test}.langid.spl


////////////////////////////////////////////////////////////////////////
// 4. WORDLISTS FOR TRANSLITERATION PRE-COMPUTATION
//  Inputs:
//      DATA/emb/{polyglot,polyglot-shared}/en.emb
//      DATA/{fire,icon,msr}/en_hi/{dev,test}.langid.spl
//      DATA/irshad/{en_hi,en_hi_normalized_oracle}/{train,dev,test}.langid.spl
//      DATA/ud/UD_English-EWT-master/{train,dev,test}.conllu
//  Outputs:
//      DATA/latn_vocab.txt
writeUsing(File(s"$datadir/translit/latn_vocab.txt")) { f =>
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
////////////////////////////////////////////////////////////////////////
// 4.
//  Inputs:
//      DATA/emb/{polyglot,polyglot-shared}/hi.emb
//      DATA/irshad/en_hi_normalized_oracle/{train,dev,test}.langid.spl
//      DATA/ud/UD_Hindi-HDTB-master/{train,dev,test}.conllu
//  Outputs:
//      DATA/deva_vocab.txt
writeUsing(File(s"$datadir/translit/deva_vocab.txt")) { f =>
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

//////////////////////////////////////////////////////////
// 5. Run experimental/users/dhgarrette/cspos/wf/make-translit-file.wf using {latn,deva}_vocab.txt
//  Inputs:
//      DATA/{latn,deva}_vocab.txt
//  Outputs:
//      DATA/translit/deva2latn.txt
//      DATA/translit/latn2deva.txt
//////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////
// 6. WORDLISTS FOR TRANSLITERATION PRE-COMPUTATION
//  Inputs:
//      DATA/translit/deva2latn.txt
//      DATA/translit/latn2deva.txt
//      DATA/translit/latn_vocab.txt
//      DATA/emb/{polyglot,polyglot-shared}/hi.emb
//  Outputs:
//      DATA/emb/{polyglot,polyglot-shared}/hi_latn.emb
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
}

////////////////////////////////////////////////////////////////////////
// 7. MAKE COMBINED EN/HI WEMB FILES FOR MIMICK TRAINING
// Combine en.emb (downloaded) and hi.emb (downloaded) into a single embeddings file for training mimick.
// Combine en.emb (downloaded) and hi_latn.emb (generated) into a single embeddings file for training mimick.
//  Inputs:
//      DATA/emb/{polyglot,polyglot-shared}/en.emb
//      DATA/emb/{polyglot,polyglot-shared}/{hi,hi_latn}.emb
//  Outputs:
//      DATA/emb/{polyglot,polyglot-shared}/{en-hi,en-hi_latn}.emb
for (embset <- Vector("polyglot", "polyglot-shared")) {
  for (ext <- Vector("", "_latn")) {
    writeUsing(File(s"$datadir/emb/$embset/en-hi$ext.emb")) { f => 
      val en = File(s"$datadir/emb/$embset/en.emb").readLines.map(_.lsplit("\\s+",2).toTuple2).toMap
      val hi = File(s"$datadir/emb/$embset/hi$ext.emb").readLines.map(_.lsplit("\\s+",2).toTuple2).toMap
      val averagedConstLines = ConstantTypes.mapTo(w => (en(w).splitWhitespace zipSafe hi(w).splitWhitespace)
                                                            .map { case (ee, he) => f"${(ee.toDouble + he.toDouble) / 2}%.7f" }
                                                            .mkString(" "))
      (averagedConstLines ++ ((en -- ConstantTypes).toVector ++ (hi -- ConstantTypes)).shuffled).map{case (w,e) => s"$w $e"}.foreach(f.writeLine)
    }
  }
}

////////////////////////////////////////////////////////////////////////
// 8. MAKE LATINIZED VERSIONS OF HINDI UD DATA
//  Inputs:
//      DATA/ud/UD_Hindi-HDTB-master/{train,dev,test}.conllu
//  Outputs:
//      DATA/ud/UD_Hindi-HDTB-master_Latn/{train,dev,test}.conllu
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

////////////////////////////////////////////////////////////////////////
// 9. MAKE COMBINED EN/HI UD TRAINING DATA
//  Inputs:
//      DATA/ud/UD_English-EWT-master/train.conllu
//      DATA/ud/UD_Hindi-HDTB-master/train.conllu
//  Outputs:
//      DATA/ud/en_hi/train.conllu
writeUsing(File(s"$datadir/ud/en_hi/train.conllu")) { f => 
  val en = File(s"$datadir/ud/UD_English-EWT-master/train.conllu").readLines.splitWhere(_.isEmpty).toVector
  val hi = File(s"$datadir/ud/UD_Hindi-HDTB-master/train.conllu").readLines.splitWhere(_.isEmpty).toVector
  (en ++ hi).shuffled.foreach(s => f.writeLine(s.mkString("\n")+"\n"))
}

////////////////////////////////////////////////////////////////////////
// 10. MAKE COMBINED EN/HI_LATN UD TRAINING DATA
//  Inputs:
//      DATA/ud/UD_English-EWT-master/train.conllu
//      DATA/ud/UD_Hindi-HDTB-master_Latn/train.conllu
//  Outputs:
//      DATA/ud/en-hi_latn/train.conllu
writeUsing(File(s"$datadir/ud/en-hi_latn/train.conllu")) { f => 
  val en = File(s"$datadir/ud/UD_English-EWT-master/train.conllu").readLines.splitWhere(_.isEmpty).toVector
  val hi = File(s"$datadir/ud/UD_Hindi-HDTB-master_Latn/train.conllu").readLines.splitWhere(_.isEmpty).toVector
  (en ++ hi).shuffled.foreach(s => f.writeLine(s.mkString("\n")+"\n"))
}

////////////////////////////////////////////////////////////////////////
// 11. MAKE UNANNOTATED TRAINING DATA TO TRAIN NGRAM LANGUAGE MODELS
//  Inputs:
//      DATA/ud/UD_English-EWT-master/train.conllu
//      DATA/ud/UD_Hindi-HDTB-master_Latn/train.conllu
//  Outputs:
//      DATA/ud/UD_English-EWT-master/train_latn.spl
//      DATA/ud/UD_Hindi-HDTB-master_Latn/train_latn.spl
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
