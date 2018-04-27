import dhg.util._
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
//      //DATA/translit/latn_vocab.txt
//      DATA/emb/{polyglot,polyglot-shared}/hi.emb
//  Outputs:
//      DATA/emb/{polyglot,polyglot-shared}/hi_latn.emb
val Vector(deva2latnTranslitDists, latn2devaTranslitDists) =
    Vector("deva2latn", "latn2deva").map { fn =>
      val allLines = File(s"$datadir/translit/$fn.txt").readLines ++
                     (if (fn=="deva2latn") Vector("। .:1") else Vector.empty) ++
                     ConstantTypes.map(w => s"$w $w|1")
      allLines.flatMap { line =>
      	  val Vector(deva, translitsString) = line.lsplit("\\s+", 2)
      	  val translits: Vector[(String, Double)] =
      	      translitsString.lsplit(",")
                .map(_.rsplit(":", 2).map(_.trim))
                .collect { case Vector(word, prob) if word.nonEmpty => (word, prob.toDouble) }
        if (translits.nonEmpty && deva.nonEmpty)
          Some(deva -> ((translits, new SimpleExpProbabilityDistribution(translits.toMap))))
        else
          None
      }.toMap
    }

//val latnVocab: Set[String] = File(s"$datadir/translit/latn_vocab.txt").readLines.filter(_.nonEmpty).flatMap(w => Set(w, w.toLowerCase, removePunc(w))).toSet
  
val Vector(enTrainWords, hiTrainWords, hiLatnTrainWords) =
    Vector("ud/UD_English-EWT-master",
           "ud/UD_Hindi-HDTB-master",
           "ud/UD_Hindi-HDTB-master_Latn").map { fn =>
      File(s"data/$fn/train.conllu").readLines.filter(_.nonEmpty).filterNot(_.startsWith("#")).map(_.splitWhitespace.apply(1)).toSet
    }

def makeVariantSamplers(devaWords: Map[String, String]) = { 
	val variants: Vector[(String, ((String, String), Double))] =  // latn -> {dist over embeddings from deva words that can be transliterated to the latn word}
  		devaWords.toVector.flatMap { case (devaWord, emb) =>
    		devaWord match {
    			case _ if ConstantTypes.contains(devaWord) =>
    		    	Set(devaWord -> ((devaWord, emb), 100.0))
    			case LatnRe() =>
      			//if (latnVocab(devaWord))
      			Set(devaWord -> ((devaWord, emb), 100.0))
      			//else
      			//   Set.empty[(String, (String, Double))]
    			case _ =>
      			deva2latnTranslitDists.get(devaWord).map { case (ts, d) =>
        			ts.zipWithIndex.flatCollect { case ((v, p), i) => //if latnVocab.contains(v) =>
          			// transliteratedLatnWord -> ((originalDevaWord, originalDevaWord's emb (if known)), transliterationProbability)
          			val wordEmbProb = Some(v -> ((devaWord, emb), p)) 
          			if (i == 0) {
          				// Always take the top transliteration
          				wordEmbProb
          			} else {
          				// For the rest, do a weighted coin flip for each weighted variant produced by the transliteration model.
          				val bernoulli = new SimpleExpProbabilityDistribution(Map(wordEmbProb -> p, None -> (100-p)))
          						bernoulli.sample()
          			}
        			}.toSet
      			}.getOrElse(Set.empty)
		  }
    }
  val variantsGrouped: Map[String, Vector[((String, String), Double)]] = variants.groupByKey
  val variantEmbSamplers: Map[String, SimpleExpProbabilityDistribution[(String, String)]] =  // latn -> {dist over embeddings from deva words that can be transliterated to the latn word} 
  		  variantsGrouped.mapVals(variantEmbs => new SimpleExpProbabilityDistribution(variantEmbs.toMap))
  variantEmbSamplers
}

for (embset <- Vector("polyglot", "polyglot-shared")) {
  val devaEmbeddings: Map[String, String] =
    File(s"$datadir/emb/$embset/hi.emb").readLines.filter(_.splitWhitespace.size > 2).map { line =>
      	line.lsplit("\\s+", 2).toTuple2
    }.toMap

  // Make a mapping from all Deva words with embeddings to their chosen Latn transliterations.
  //   In this block, each Latn word (a generated transliteration) copies the embedding of exactly one Deva word.
  //   This means that a Deva word can be mapped to multiple Latn words, but no Latn word will be mapped to by multiple Deva words.
  //   It is not necessarily the case that every Deva word will appear.
  val latn2DevaEmbMapping: Map[String, String] = {
    writeUsing(File(s"$datadir/emb/$embset/hi_latn.emb")) { f =>
      val variantEmbSamplers = makeVariantSamplers(devaEmbeddings)
      for (latnWord <- ConstantTypes) {
        variantEmbSamplers.get(latnWord).foreach { embDist =>
          val (devaWord, emb) = embDist.sample()
      	    f.writeLine(s"$latnWord $emb")
        }
      }
      for {
        (latnWord, embDist) <- variantEmbSamplers
        if latnWord.nonEmpty
        if !ConstantTypes.contains(latnWord)
      } yield {
        val (devaWord, emb) = embDist.sample()
    	    f.writeLine(s"$latnWord $emb")
    	    (latnWord, devaWord)
      }
    }
  }
  val latn2DevaTrainWordMapping: Map[String, String] = {
    val variantTrainWordSamplers = makeVariantSamplers(hiTrainWords.mapToVal("").toMap)
    for {
      (latnWord, embDist) <- variantEmbSamplers
      if latnWord.nonEmpty
      if !ConstantTypes.contains(latnWord)
      if !latn2DevaEmbMapping.contains(latnWord)
    } yield {
      val (devaWord, emb) = embDist.sample()
  	    (latnWord, devaWord)
    }
  }
  val enEmbWords: Set[String] = File(s"$datadir/emb/$embset/en.emb").readLines.filter(_.splitWhitespace.size > 2).map(_.lsplit("\\s+", 2).head).toSet
  writeUsing(File(s"$datadir/emb/$embset/translit_mapping.txt")) { mapping_file =>
    val constantWords: Map[String, Set[String]] =
        ConstantTypes.mapTo(Set(_)).toMap ++
      		ConstantTypes.map(w => ("hi:"+w) -> Set(w)) ++
      		ConstantTypes.map(w => ("en:"+w) -> Set(w))
  		val mappings: Map[String, Set[String]] = 
		    (hiLatnTrainWords.map(w => ("hi:"+w) -> Set(w)).toMap ++
         (latn2DevaTrainWordMapping ++ latn2DevaEmbMapping).toSet[(String,String)].map(_.swap).groupByKey.mapKeys("hi:"+_) ++
         (enTrainWords ++ enEmbWords).map(w => ("en:"+w) -> Set(w))) -- constantWords.keys
    for ((origWord, translits) <- Vector(constantWords, mappings).map(_.toVector.sortBy(_._1)).flatten) {
      mapping_file.writeLine(s"$origWord ${translits.toVector.sorted.mkString(" ")}")
    }
  }
}

  
////////////////////////////////////////////////////////////////////////
// 13. Prefix all words in embeddings files so we can look up language-specific vectors.
for {
  (lang_code, langdir) <- Vector(("en", "en"),
                                 ("hi", "hi"),
                                 ("hi", "hi_latn"))
  dir <- Vector("polyglot", "polyglot-shared")
} {
  writeUsing(File(s"$datadir/emb/$dir/$langdir-prefixed.emb")) { f =>
    val inputFile = s"$datadir/emb/$dir/$langdir.emb"
    println(inputFile)
    for (line <- File(inputFile).readLines.filter(_.splitWhitespace.size > 2)) {
      f.writeLine(lang_code + ":" + line)
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
for {
  embset <- Vector("polyglot", "polyglot-shared")
  ext <- Vector("", "_latn")
  pref <- Vector("", "-prefixed")
} {
  writeUsing(File(s"$datadir/emb/$embset/en-hi$ext$pref.emb")) { f => 
    val Vector(en, hi) =
      Vector(s"$datadir/emb/$embset/en$pref.emb",
             s"$datadir/emb/$embset/hi$ext$pref.emb").map(File(_))
          .map(_.readLines.filter(_.splitWhitespace.size > 2).map(_.lsplit("\\s+",2).toTuple2).toMap)
    val averagedConstLines = ConstantTypes.mapTo { w =>
      (en(if (pref=="-prefixed") "en:"+w else w).splitWhitespace zipSafe hi(if (pref=="-prefixed") "hi:"+w else w).splitWhitespace)
          .map { case (ee, he) => f"${(ee.toDouble + he.toDouble) / 2}%.7f" }
          .mkString(" ")
    }
    (averagedConstLines ++ ((en -- ConstantTypes).toVector ++ (hi -- ConstantTypes)).shuffled).map{case (w,e) => s"$w $e"}.foreach(f.writeLine)
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
        //val Vector(goldTranslit) = splitLine.last.lsplit("\\|").map(_.lsplit("=")).collectFirst { case Vector("Translit", value) => value }
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
// 9. Prefix all words in train data files so we can look up their language-specific vectors.
for ((lang_code, langdir) <- Vector(("en", "UD_English-EWT-master"),
                                    ("hi", "UD_Hindi-HDTB-master"),
                                    ("hi", "UD_Hindi-HDTB-master_Latn"))) {
  for (dataset <- Vector("train", "dev", "test")) {
    writeUsing(File(s"$datadir/ud/$langdir/$dataset-prefixed.conllu")) { f =>
      val inputFile = s"$datadir/ud/$langdir/$dataset.conllu"
      println(inputFile)
      for (line <- File(inputFile).readLines) {
        val outLine =
          if (line.isEmpty) line  
          else if (line.startsWith("#")) line
          else {
            val split = line.splitWhitespace
            (split.take(1) ++ split.slice(1,3).map(lang_code+":"+_) ++ split.drop(3)).mkString("\t")
          }
        f.writeLine(outLine)
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////
// 10. MAKE COMBINED EN/HI UD TRAINING DATA
//  Inputs:
//      DATA/ud/UD_English-EWT-master/train.conllu
//      DATA/ud/UD_Hindi-HDTB-master/train.conllu
//      DATA/ud/UD_Hindi-HDTB-master_Latn/train.conllu
//      DATA/ud/UD_English-EWT-master/train-prefixed.conllu
//      DATA/ud/UD_Hindi-HDTB-master/train-prefixed.conllu
//      DATA/ud/UD_Hindi-HDTB-master_Latn/train-prefixed.conllu
//  Outputs:
//      DATA/ud/en_hi/train.conllu
//      DATA/ud/en-hi_latn/train.conllu
//      DATA/ud/en_hi/train-prefixed.conllu
//      DATA/ud/en-hi_latn/train-prefixed.conllu
for {
  dataset <- Vector("train", "dev", "test")
  (outFile, enFile, hiFile) <-
    Vector((s"$datadir/ud/en_hi/$dataset.conllu",
            s"$datadir/ud/UD_English-EWT-master/$dataset.conllu",
            s"$datadir/ud/UD_Hindi-HDTB-master/$dataset.conllu"),
           (s"$datadir/ud/en-hi_latn/$dataset.conllu",
            s"$datadir/ud/UD_English-EWT-master/$dataset.conllu",
            s"$datadir/ud/UD_Hindi-HDTB-master_Latn/$dataset.conllu"),
           (s"$datadir/ud/en_hi/$dataset-prefixed.conllu",
            s"$datadir/ud/UD_English-EWT-master/$dataset-prefixed.conllu",
            s"$datadir/ud/UD_Hindi-HDTB-master/$dataset-prefixed.conllu"),
           (s"$datadir/ud/en-hi_latn/$dataset-prefixed.conllu",
            s"$datadir/ud/UD_English-EWT-master/$dataset-prefixed.conllu",
            s"$datadir/ud/UD_Hindi-HDTB-master_Latn/$dataset-prefixed.conllu"))
} {
  writeUsing(File(outFile)) { f =>
    Vector(enFile, hiFile).map(File(_))
        .flatMap(_.readLines.splitWhere(_.isEmpty).toVector)
        .shuffled
        .foreach(s => f.writeLine(s.mkString("\n")+"\n"))
  }
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

////////////////////////////////////////////////////////////
// CHECK TO SEE IF THE SAME TAGSET IS USED EVERYWHERE
//val ValidPos = "., ADJ, ADP, ADV, AUX, CCONJ, CONJ, DET, INTJ, NOUN, NUM, PART, PRON, PROPN, PRT, PUNCT, SCONJ, SYM, VERB, X".lsplit(", ")   // 20
  val ValidPos =    "ADJ, ADP, ADV, AUX, CCONJ,       DET, INTJ, NOUN, NUM, PART, PRON, PROPN,      PUNCT, SCONJ, SYM, VERB, X".lsplit(", *")  // 17
val filePosCounts: Vector[(String, Map[String, Int])] =
  Vector(
//      "icon/en_hi/dev.conllu",
//      "icon/en_hi/test.conllu",
      "irshad/en_hi/dev.conllu",
      "irshad/en_hi/test.conllu",
      "irshad/en_hi_normalized_oracle/dev.conllu",
      "irshad/en_hi_normalized_oracle/test.conllu",
//      "msr/en_hi/dev.conllu",
//      "msr/en_hi/test.conllu",
      "ud/en_hi/train.conllu",
      "ud/en-hi_latn/train.conllu",
      "ud/UD_English-EWT-master/train.conllu",
      "ud/UD_English-EWT-master/dev.conllu",
      "ud/UD_English-EWT-master/test.conllu",
      "ud/UD_Hindi-HDTB-master/train.conllu",
      "ud/UD_Hindi-HDTB-master/dev.conllu",
      "ud/UD_Hindi-HDTB-master/test.conllu",
      "ud/UD_Hindi-HDTB-master_Latn/train.conllu",
      "ud/UD_Hindi-HDTB-master_Latn/dev.conllu",
      "ud/UD_Hindi-HDTB-master_Latn/test.conllu",
//      "ud/UD_Spanish-GSD-master/train.conllu",
//      "ud/UD_Spanish-GSD-master/dev.conllu",
//      "ud/UD_Spanish-GSD-master/test.conllu",
  ).mapTo { fn =>
    File(s"data/$fn").readLines.zipWithIndex.filterNot(_._1.startsWith("#")).filter(_._1.nonEmpty).map { case (line, i) =>
      val pos = line.splitWhitespace.apply(3) match {
//        case "." => "PUNCT"
        case p => p
      }
      if (!ValidPos.contains(pos)) {
        println(s"$fn :: ${i+1} :: $line")
      }
      pos
    }.counts
  }
val allPos: Vector[String] = filePosCounts.flatMap(_._2.keySet).sorted.distinct
{
println(f"${""}%-45s${allPos.map(pos => f"$pos%5s").mkString(" ")}")
filePosCounts.foreach { case (fn: String, posCounts: Map[String, Int]) =>
  println(f"$fn%-45s${allPos.map { pos: String =>
    val pctOpt: Option[Double] = posCounts.normalizeValues.get(pos)
    f"${pctOpt.map(v => f"$v%.2f").getOrElse("0")}%5s"
  }.mkString(" ")}")
}
}
