writeUsing(File("/Users/dhgarrette/workspace/unsupervised-codemixing/data/msr/en_hi/dev.spl")) { f=>
 File("/Users/dhgarrette/workspace/unsupervised-codemixing/data/msr/en_hi/tagged-msr-hi_en-orig-coarse-dev.txt").readLines.foreach { line =>
  f.writeLine(line.splitWhitespace.map { _.rsplit(raw"\|") match {
    case Vector(word, "en", _) => s"$word|en"
    case Vector(word, "hi", _) => s"$word|es"
    case Vector(word, lang, _) => s"$word|$lang"
   }
  }.mkString(" "))
 }
}


writeUsing(File("/Users/dhgarrette/workspace/unsupervised-codemixing/deva_wordlist-emb.txt")) { f=>
 for (fn <- Vector("/Users/dhgarrette/workspace/codemix-pos/data/clean/emb-polyglot-hi.txt")) {
  println(fn)
  File(fn).readLines.map(_.splitWhitespace(0)).toVector.sorted.distinct.foreach(f.writeLine)
 }
}

writeUsing(File("/Users/dhgarrette/workspace/unsupervised-codemixing/data/msr/en_hi/deva_wordlist.txt")) { f=>
 for (fn <- Vector("/Users/dhgarrette/workspace/unsupervised-codemixing/data/msr/en_hi/MSR-POSData.txt")) {
  println(fn)
  File(fn).readLines.flatMap(_.splitWhitespace.map(_.rsplit("=")).collect { case Vector(_, deva) if deva.trim.nonEmpty => deva.trim }).toVector.sorted.distinct.foreach(f.writeLine)
 }
}

val NumRe = raw"\d+".r
writeUsing(File("/Users/dhgarrette/workspace/unsupervised-codemixing/data/irshad/en_hi/deva_wordlist.txt")) { f=>
 for (fn <- Vector("/Users/dhgarrette/workspace/unsupervised-codemixing/data/irshad/original-en_hi/dev.conllu",
                   "/Users/dhgarrette/workspace/unsupervised-codemixing/data/irshad/original-en_hi/test.conllu")) {
  println(fn)
  File(fn).readLines.map(_.trim).collect {
   case line if !line.trim.startsWith("#") && line.nonEmpty =>
    line.splitWhitespace(1)
    //.map { line =>
     //line.replaceAll(raw"[^(),.]+", "")
    //}.filter { case NumRe() => false; case _ => true }
  }
 }.toVector.sorted.distinct.foreach(f.writeLine)
}

writeUsing(File("/Users/dhgarrette/workspace/unsupervised-codemixing/data/translit/latn_vocab.txt")) { f =>
 val splWords: Set[String] =
   Set("/Users/dhgarrette/workspace/unsupervised-codemixing/data/fire/en_hi/dev.spl",
       "/Users/dhgarrette/workspace/unsupervised-codemixing/data/fire/en_hi/test.spl",
       "/Users/dhgarrette/workspace/unsupervised-codemixing/data/icon/en_hi/dev.spl",
       "/Users/dhgarrette/workspace/unsupervised-codemixing/data/icon/en_hi/test.spl",
       "/Users/dhgarrette/workspace/unsupervised-codemixing/data/irshad/en_hi/dev.spl",
       "/Users/dhgarrette/workspace/unsupervised-codemixing/data/irshad/en_hi/test.spl",
       "/Users/dhgarrette/workspace/unsupervised-codemixing/data/msr/en_hi/dev.spl",
       "/Users/dhgarrette/workspace/unsupervised-codemixing/data/msr/en_hi/test.spl").flatMap { fn =>
    println(fn)
    File(fn).readLines.flatMap(_.splitWhitespace.map(_.rsplit(raw"\|",2).head))
   }
 val conllWords: Set[String] =
   Set("/Users/dhgarrette/workspace/unsupervised-codemixing/data/ud/UD_English-EWT-master/en-ud-dev.conllu",
       "/Users/dhgarrette/workspace/unsupervised-codemixing/data/ud/UD_English-EWT-master/en-ud-test.conllu",
       "/Users/dhgarrette/workspace/unsupervised-codemixing/data/ud/UD_English-EWT-master/en-ud-train.conllu").flatMap { fn =>
    println(fn)
    File(fn).readLines.filterNot(_.startsWith("#")).filter(_.nonEmpty).map(_.splitWhitespace(1))
   }
 val embWords: Set[String] =
    Set("/Users/dhgarrette/workspace/codemix-pos/data/clean/emb-polyglot-en.txt").flatMap { fn =>
     println(fn)
     File(fn).readLines.map(_.splitWhitespace(0))
    }
 (splWords ++ conllWords ++ embWords).toVector.sorted.distinct.foreach(f.writeLine)
}

writeUsing(File("/Users/dhgarrette/workspace/unsupervised-codemixing/data/translit/deva_vocab.txt")) { f =>
 val conllWords: Set[String] =
   Set("/Users/dhgarrette/workspace/unsupervised-codemixing/data/ud/UD_Hindi-HDTB-master/hi-ud-dev.conllu",
       "/Users/dhgarrette/workspace/unsupervised-codemixing/data/ud/UD_Hindi-HDTB-master/hi-ud-test.conllu",
       "/Users/dhgarrette/workspace/unsupervised-codemixing/data/ud/UD_Hindi-HDTB-master/hi-ud-train.conllu").flatMap { fn =>
    println(fn)
    File(fn).readLines.filterNot(_.startsWith("#")).filter(_.nonEmpty).map(_.splitWhitespace(1))
   }
 val embWords: Set[String] =
    Set("/Users/dhgarrette/workspace/codemix-pos/data/clean/emb-polyglot-hi.txt").flatMap { fn =>
     println(fn)
     File(fn).readLines.map(_.splitWhitespace(0))
    }
 (conllWords ++ embWords).toVector.sorted.distinct.foreach(f.writeLine)
}




writeUsing(File("/Users/dhgarrette/workspace/unsupervised-codemixing/data/ud/UD_English-EWT-master/train_latn.spl")) { f=>
 Vector("/Users/dhgarrette/workspace/unsupervised-codemixing/data/ud/UD_English-EWT-master/en-ud-train.conllu").foreach { fn =>
  println(fn)
  File(fn).readLines.filterNot(_.startsWith("#")).splitWhere(_.trim.isEmpty).foreach { sentenceLines => 
      f.writeLine(sentenceLines.map(_.splitWhitespace(1).toLowerCase).mkString(" "))
    }
 }
}

writeUsing(File("/Users/dhgarrette/workspace/unsupervised-codemixing/data/ud/UD_Hindi-HDTB-master/train_latn.spl")) { f=>
 val wordCounts: Map[String, Int] = 
   Vector("/Users/dhgarrette/workspace/unsupervised-codemixing/data/ud/UD_Hindi-HDTB-master/hi-ud-train.conllu").flatMap { fn =>
      println(fn)
      File(fn).readLines.filterNot(_.startsWith("#")).filter(_.nonEmpty).map(_.splitWhitespace(1))
     }.map { _.toLowerCase }.counts
 val samplers =
   File("/Users/dhgarrette/workspace/unsupervised-codemixing/data/translit/deva2latn.txt").readLines.map(_.splitWhitespace)
     .collect { case input @ Vector(word, translitsString) if wordCounts.contains(word)  =>
       println(input)
       val d = new SimpleLogProbabilityDistribution[String](
                         translitsString
                             .lsplit(",")
                             .map(_.rsplit(":",2))
                             .map { case Seq(variant, prob) => (variant, LogDouble(prob.toDouble / 100)) }.toMap)
       (word, d)
     }.toMap
 Vector("/Users/dhgarrette/workspace/unsupervised-codemixing/data/ud/UD_Hindi-HDTB-master/hi-ud-train.conllu").foreach { fn =>
    println(fn)
    File(fn).readLines.filterNot(_.startsWith("#")).splitWhere(_.trim.isEmpty).foreach { sentenceLines => 
      f.writeLine(sentenceLines.map(_.splitWhitespace(1).toLowerCase).flatMap(samplers.get).map(_.sample()).mkString(" "))
    }
   }
}


val ConstantTypes = Vector("</S> </S>|1", "<UNK> <UNK>|1", "<PAD> <PAD>|1", "<S> <S>|1")

import nlp.hmm.prob.SimpleExpProbabilityDistribution
val deva2latnTranslitDists =
  (File("/Users/dhgarrette/workspace/unsupervised-codemixing/data/translit/deva2latn.txt").readLines ++ Vector("। .:1") ++ ConstantTypes).flatMap { line =>
  	  val Vector(deva, translitsString) = line.lsplit("\\s+", 2)
  	  val translits = translitsString.lsplit(",")
                        .map(_.rsplit(":", 2).map(_.trim))
                        .collect { case Vector(word, prob) => (word, prob.toDouble) }
                        .toVector
    if (translits.nonEmpty)
      Some(deva -> (translits, new SimpleExpProbabilityDistribution(translits.toMap)))
    else
      None
  }.toMap

import nlp.hmm.prob.SimpleExpProbabilityDistribution
val latn2devaTranslitDists =
  (File("/Users/dhgarrette/workspace/unsupervised-codemixing/data/translit/latn2deva.txt").readLines ++ ConstantTypes).flatMap { line =>
  	  val Vector(deva, translitsString) = line.lsplit("\\s+", 2)
  	  val translits = translitsString.lsplit(",")
                        .map(_.lsplit(":").map(_.trim))
                        .collect { case Vector(word, prob) => (word, prob.toDouble) }
                        .toVector
    if (translits.nonEmpty)
      Some(deva -> (translits, new SimpleExpProbabilityDistribution(translits.toMap)))
    else
      None
  }.toMap

val devaEmbeddings: Map[String, String] =
  File("/Users/dhgarrette/workspace/unsupervised-codemixing/data/emb/polyglot-shared/hi.emb").readLines.map { line =>
    	line.lsplit("\\s+", 2).toTuple2
  }.toMap

val latnVocab: Set[String] = File("/Users/dhgarrette/workspace/unsupervised-codemixing/data/translit/latn_vocab.txt").readLines.flatMap(w => Set(w, w.toLowerCase)).toSet

import nlp.hmm.prob.SimpleExpProbabilityDistribution
val LatnRe = raw"[\d\p{Punct}A-za-z]+".r
writeUsing(File("/Users/dhgarrette/workspace/unsupervised-codemixing/data/emb/polyglot-shared/hi_latn.emb")) { f =>
  val ConstantTypes = Vector("<UNK>", "<S>", "</S>", "<PAD>")
  val variants: Iterator[(String, (String, Double))] =  // latn -> {dist over embeddings from deva words that can be transliterated to the latn word} 
    devaEmbeddings.iterator.flatMap { case (originalWord, emb) =>
      originalWord match {
      	  case _ if ConstantTypes.contains(originalWord) =>
      	    Set(originalWord -> (emb, 100.0))
        case LatnRe() =>
          //if (latnVocab(originalWord))
            Set(originalWord -> (emb, 100.0))
          //else
          //   Set.empty[(String, (String, Double))]
        case _ =>
          deva2latnTranslitDists.get(originalWord).map { case (ts, d) =>
            val tsToKeep =
              ts.collect { case (v, p) => //if latnVocab.contains(v) =>
                // Do a weighted coin flip for each weighted variant produced by the transliteration model.
                new SimpleExpProbabilityDistribution(Map(Some(v -> (emb, p)) -> p, None -> (100-p))).sample()
              }.flatten
            //if (tsToKeep.size > 1)
            //  println(s"tsToKeep: ${originalWord} -> ${tsToKeep.map(_._1)}")
            tsToKeep
          }.getOrElse(Set.empty)
      }
    }
  val variantEmbSamplers: Map[String, SimpleExpProbabilityDistribution[String]] =  // latn -> {dist over embeddings from deva words that can be transliterated to the latn word} 
      variants.groupByKey.mapVals(variantEmbs => new SimpleExpProbabilityDistribution(variantEmbs.toMap))
  println(s"variantEmbSamplers.size() = ${variantEmbSamplers.size}")
  for (latnWord <- ConstantTypes) {
    println("TRYING TO HANDLE CONSTANT " + latnWord)
    variantEmbSamplers.get(latnWord).foreach { d =>
	    f.writeLine(s"$latnWord ${d.sample()}")
	    println("HANDLED CONSTANT " + latnWord)
    }
  }
  for ((latnWord, embDist) <- variantEmbSamplers) {
    if (!ConstantTypes.contains(latnWord)) {
	    f.writeLine(s"$latnWord ${embDist.sample()}")
    }
  }
}

val OkayRe = raw"[\d\p{Punct}]+".r
for (set <- Vector("train", "dev", "test")) {
  writeUsing(File(s"/Users/dhgarrette/workspace/unsupervised-codemixing/data/ud/UD_Hindi-HDTB-master/hi_latn-ud-$set.conllu")) { f =>
    for (line <- File(s"/Users/dhgarrette/workspace/unsupervised-codemixing/data/ud/UD_Hindi-HDTB-master/hi-ud-$set.conllu").readLines) {
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

writeUsing(File("/Users/dhgarrette/workspace/unsupervised-codemixing/data/ud/en_hi/train.conll")) { f => 
  val en = File("/Users/dhgarrette/workspace/unsupervised-codemixing/data/ud/UD_English-EWT-master/en-ud-train.conllu").readLines.splitWhere(_.isEmpty).toVector
  val hi = File("/Users/dhgarrette/workspace/unsupervised-codemixing/data/ud/UD_Hindi-HDTB-master/hi-ud-train.conllu").readLines.splitWhere(_.isEmpty).toVector
  (en ++ hi).shuffle.foreach(s => f.writeLine(s.mkString("\n")+"\n"))
}

writeUsing(File("/Users/dhgarrette/workspace/unsupervised-codemixing/data/emb/polyglot-shared/en_hi_latn.emb")) { f => 
  val en = File("/Users/dhgarrette/workspace/unsupervised-codemixing/data/emb/polyglot-shared/en.emb").readLines.map(_.lsplit("\\s+",2).toTuple2).toMap
  val hi = File("/Users/dhgarrette/workspace/unsupervised-codemixing/data/emb/polyglot-shared/hi_latn.emb").readLines.map(_.lsplit("\\s+",2).toTuple2).toMap
  val consts = Vector("<UNK>", "<S>", "</S>", "<PAD>")
  val averagedConstLines = consts.mapTo(w => (en(w).splitWhitespace zipSafe hi(w).splitWhitespace).map { case (ee, he) => f"${(ee.toDouble + he.toDouble) / 2}%.7f" }.mkString(" "))
  (averagedConstLines ++ ((en -- consts).toVector ++ (hi -- consts)).shuffle).map{case (w,e) => s"$w $e"}.foreach(f.writeLine)
}
