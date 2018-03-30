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
 for (fn <- Vector("/Users/dhgarrette/workspace/unsupervised-codemixing/data/irshad/en_hi/TWEETS-dev-v2_utf.conllu",
                   "/Users/dhgarrette/workspace/unsupervised-codemixing/data/irshad/en_hi/TWEETS-test-v2_utf.conllu")) {
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


writeUsing(File("/Users/dhgarrette/workspace/unsupervised-codemixing/data/latn_wordlist_spl.txt")) { f=>
 Vector("/Users/dhgarrette/workspace/unsupervised-codemixing/data/fire/en_hi/dev.spl",
        "/Users/dhgarrette/workspace/unsupervised-codemixing/data/fire/en_hi/test.spl",
        "/Users/dhgarrette/workspace/unsupervised-codemixing/data/icon/en_hi/dev.spl",
        "/Users/dhgarrette/workspace/unsupervised-codemixing/data/icon/en_hi/test.spl",
        "/Users/dhgarrette/workspace/unsupervised-codemixing/data/irshad/en_hi/dev.spl",
        "/Users/dhgarrette/workspace/unsupervised-codemixing/data/irshad/en_hi/test.spl",
        "/Users/dhgarrette/workspace/unsupervised-codemixing/data/msr/en_hi/dev.spl",
        "/Users/dhgarrette/workspace/unsupervised-codemixing/data/msr/en_hi/test.spl").flatMap { fn =>
  println(fn)
  File(fn).readLines.flatMap(_.splitWhitespace.map(_.rsplit(raw"\|",2).head))
 }.collect { case w @ WordRe() => w.toLowerCase }.collect { case w @ WordRe() => w }.toVector.sorted.distinct.foreach(f.writeLine)
}

val WordRe = raw"[a-z]+".r
writeUsing(File("/Users/dhgarrette/workspace/unsupervised-codemixing/data/latn_wordlist_ud.txt")) { f=>
 Vector("/Users/dhgarrette/workspace/unsupervised-codemixing/data/ud/UD_English-EWT-master/en-ud-dev.conllu",
        "/Users/dhgarrette/workspace/unsupervised-codemixing/data/ud/UD_English-EWT-master/en-ud-test.conllu",
        "/Users/dhgarrette/workspace/unsupervised-codemixing/data/ud/UD_English-EWT-master/en-ud-train.conllu").flatMap { fn =>
  println(fn)
  File(fn).readLines.filterNot(_.startsWith("#")).filter(_.nonEmpty).map(_.splitWhitespace(1))
 }.collect { case w @ WordRe() => w.toLowerCase }.toVector.sorted.distinct.foreach(f.writeLine)
}

writeUsing(File("/Users/dhgarrette/workspace/unsupervised-codemixing/data/latn_wordlist-emb.txt")) { f=>
 Vector("/Users/dhgarrette/workspace/codemix-pos/data/clean/emb-polyglot-en.txt").flatMap { fn =>
  println(fn)
  File(fn).readLines.map(_.splitWhitespace(0))
 }.collect { case w @ WordRe() => w }.toVector.sorted.distinct.foreach(f.writeLine)
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
