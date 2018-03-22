package nlp.hmm.run

import dhg.util._
import nlp.hmm.prob.LaplaceLogProbabilityDistribution
import nlp.hmm.tag.learn.UnsmoothedTransitionDistributioner
import nlp.hmm.tag.learn.UnsmoothedEmissionDistributioner
import nlp.hmm.tag.learn.SoftEmHmmTaggerTrainer
import nlp.hmm.prob.SimpleConditionalLogProbabilityDistribution
import nlp.hmm.prob.SimpleLogProbabilityDistribution
import nlp.hmm.prob.LogProbabilityDistribution
import nlp.hmm.tagdict.SimpleTagDictionary

object Codemix {
  type Word = String
  type Tag = String

  def countConlluRawWords(filenames: Vector[String]): Map[String, Int] = {
    filenames.flatMap { filename =>
      File(filename).readLines.flatMap { line =>
        if (line.trim.isEmpty) None
        else if (line.trim.startsWith("#")) None
        else Some(line.splitWhitespace(1).toLowerCase)
      }
    }.counts
  }

  def wordCountsToCharNgramCounts(n: Int, wordCounts: Map[String, Int]): Map[Vector[String], Map[String, Int]] = {
    wordCounts.toVector
      .flatMap { case (word, wordCount) =>
        val chars = Vector.fill(n - 1)("<S>") ++ word.map(_.toString) ++ Vector("<E>")
        chars.sliding(n).map { case context :+ c => (context, (c, wordCount)) }
      }.groupByKey
      .mapVals(_.groupByKey.mapVals(_.sum))
  }
  
  def makeNgramModel(maxN: Int, wordCounts: Map[String, Int]) = {
    val models =
      (maxN downto 1).toVector.map { n =>
        val conditionalNgramCounts = wordCountsToCharNgramCounts(n, wordCounts)
        conditionalNgramCounts.mapVals { ngramCounts =>
          new LaplaceLogProbabilityDistribution[String](
              ngramCounts.mapVals(LogDouble(_)), None, None, LogDouble(1), LogDouble(1))
        }
      }
    val unnormalizedWeights = Vector.tabulate(maxN)(n => 1.0 / (0.5 ** n))
    val normalizedWeights = unnormalizedWeights.map(_ / unnormalizedWeights.sum)
    models zipSafe normalizedWeights
  }
  
  def readLangTaggedSplFile(filenames: Vector[String]) = {
    filenames.flatMap { filename =>
      File(filename).readLines.map { line =>
        line.splitWhitespace.map(_.rsplit(raw"\|", 2) match {
          case Vector(word, "en") => (word.toLowerCase, "E")
          case Vector(word, "es") => (word.toLowerCase, "S")
          case Vector(word, lang) => (word.toLowerCase, lang)
        })
      }
    }
  }
  
  def main(args: Array[String]) = {
    // Tags:  <S>, <E>, E, xE, S, xS 
    val initTrans = new SimpleConditionalLogProbabilityDistribution(
        Map[Tag, LogProbabilityDistribution[Tag]](
            "<S>" -> new SimpleLogProbabilityDistribution(Map(
                "E"  -> LogDouble(0.25),
                "xE" -> LogDouble(0.25),
                "S"  -> LogDouble(0.25),
                "xS" -> LogDouble(0.25))),
            "<E>" -> new SimpleLogProbabilityDistribution(Map()),
            "E" -> new SimpleLogProbabilityDistribution(Map(
                "E"  -> LogDouble(0.65),
                "xE" -> LogDouble(0.24),
                "S"  -> LogDouble(0.01),
                "<E>" -> LogDouble(0.10))),
            "xE" -> new SimpleLogProbabilityDistribution(Map(
                "E"  -> LogDouble(0.80),
                "xE" -> LogDouble(0.15),
                "S"  -> LogDouble(0.05),
                "<E>" -> LogDouble(0.10))),
            "S" -> new SimpleLogProbabilityDistribution(Map(
                "E"  -> LogDouble(0.01),
                "S"  -> LogDouble(0.65),
                "xS" -> LogDouble(0.24),
                "<E>" -> LogDouble(0.10))),
            "xS" -> new SimpleLogProbabilityDistribution(Map(
                "E"  -> LogDouble(0.05),
                "S"  -> LogDouble(0.80),
                "xS" -> LogDouble(0.15),
                "<E>" -> LogDouble(0.10))),
            ))

    val monoEnWordCounts = countConlluRawWords(Vector("data/ud/UD_English-EWT-master/en-ud-train.conllu"))
    val monoEsWordCounts = countConlluRawWords(Vector("data/ud/UD_Spanish-GSD-master/es-ud-train.conllu"))
    val csTrainData = readLangTaggedSplFile(Vector("data/wcacs16/en_es/train.spl"))
    val csEvalData = readLangTaggedSplFile(Vector("data/wcacs16/en_es/dev.spl"))
    
    val PuncOnlyRe = raw"[^A-Za-z0-9]+".r
    val AtMentionRe = raw"@[A-Za-z0-9]+".r
    def isX(word: String) = word match {
      case PuncOnlyRe() => true
      case AtMentionRe() => true
      case _ => false
    }
    def isNonX(word: String) = !isX(word)
//    def getXs(csData: Vector[Vector[(String, String)]]): Set[String] = csData.flatten.map(_._1).filter(isX).toSet
//    def getNonXs(csData: Vector[Vector[(String, String)]]): Set[String] = csData.flatten.map(_._1).filter(isNonX).toSet

    val csAllTypes = (csTrainData ++ csEvalData).flatten.map(_._1).toSet
    val csGoodTypes = csTrainData.flatten.map(_._1).counts.collect{ case (word, count) if count > 1 => word }.toSet
    val monoEnOnlyTypes = monoEnWordCounts.keySet -- monoEsWordCounts.keySet
    val monoEsOnlyTypes = monoEnWordCounts.keySet -- monoEsWordCounts.keySet
    val csUnkTypes = csAllTypes -- csGoodTypes -- monoEnOnlyTypes -- monoEsOnlyTypes

//    val csAllWords = getNonXs(csTrainData ++ csEvalData)
//    val csAllXs = getXs(csTrainData ++ csEvalData)
//    val csGoodWords = csGoodTypes.filter(isNonX) // Words that should not be UNKed
//    val csGoodXs = csGoodTypes.filter(isX)  // Words that should not be UNKed
//    val csAllTrainWords = getNonXs(csTrainData)
//    val csAllEvalWords = getNonXs(csEvalData)
//    val csGoodEvalWords = csAllEvalWords & csGoodTrainWords  // Words that should not be UNKed
//    val csGoodXs = csAllXs & csGoodTrainTypes
    
//    for (w <- (csTrainData ++ csEvalData).flatten.map { case (word, lang) => (word, lang != "E" && lang != "S") }.groupByKey.mapValues(_.counts).collect { case (word, valueCounts) if valueCounts.size > 1 => (word, valueCounts(true), valueCounts(false)) }.toVector.sorted)
//      println(s"ambiguous: $w")

    val initEmiss = new SimpleConditionalLogProbabilityDistribution(
        Map[Tag, LogProbabilityDistribution[Word]](
            "<S>" -> new SimpleLogProbabilityDistribution(Map("<S>" -> LogDouble(1))),
            "<E>" -> new SimpleLogProbabilityDistribution(Map("<E>" -> LogDouble(1))),
//            "E"  -> new SimpleLogProbabilityDistribution(csAllTypes.filter(isNonX).mapTo(w => LogDouble(monoEnWordCounts.getOrElse(w, 0) + 1.0)).toMap),
//            "S"  -> new SimpleLogProbabilityDistribution(csAllTypes.filter(isNonX).mapTo(w => LogDouble(monoEsWordCounts.getOrElse(w, 0) + 1.0)).toMap),
//            "xE" -> new SimpleLogProbabilityDistribution(csAllTypes.filter(isX).mapTo(w => LogDouble(monoEnWordCounts.getOrElse(w, 0) + 1.0)).toMap),
//            "xS" -> new SimpleLogProbabilityDistribution(csAllTypes.filter(isX).mapTo(w => LogDouble(monoEsWordCounts.getOrElse(w, 0) + 1.0)).toMap),
            "E"  -> new LaplaceLogProbabilityDistribution(monoEnWordCounts.collect { case (word, count) if isNonX(word) => (word, LogDouble(count)) }, None, excludedBs = Some(Set("<S>", "<E>")), LogDouble(1.0), LogDouble(1.0)),
            "S"  -> new LaplaceLogProbabilityDistribution(monoEsWordCounts.collect { case (word, count) if isNonX(word) => (word, LogDouble(count)) }, None, excludedBs = Some(Set("<S>", "<E>")), LogDouble(1.0), LogDouble(1.0)),
            "xE"  -> new LaplaceLogProbabilityDistribution(monoEnWordCounts.collect { case (word, count) if isX(word) => (word, LogDouble(count)) }, None, excludedBs = Some(Set("<S>", "<E>")), LogDouble(1.0), LogDouble(1.0)),
            "xS"  -> new LaplaceLogProbabilityDistribution(monoEsWordCounts.collect { case (word, count) if isX(word) => (word, LogDouble(count)) }, None, excludedBs = Some(Set("<S>", "<E>")), LogDouble(1.0), LogDouble(1.0)),
        ))
//    val initEmiss = new SimpleConditionalLogProbabilityDistribution(
//        Map[Tag, LogProbabilityDistribution[Word]](
//            "<S>" -> new SimpleLogProbabilityDistribution(Map("<S>" -> LogDouble(1))),
//            "<E>" -> new SimpleLogProbabilityDistribution(Map("<E>" -> LogDouble(1))),
//            "E"  -> new SimpleLogProbabilityDistribution(Map("the" -> LogDouble(0.2), "cat" -> LogDouble(0.2), "walks" -> LogDouble(0.2), "el" -> LogDouble(0.1), "gato" -> LogDouble(0.1), "marche" -> LogDouble(0.1))),
//            "xE" -> new SimpleLogProbabilityDistribution(Map("the" -> LogDouble(0.2), "cat" -> LogDouble(0.2), "walks" -> LogDouble(0.2), "el" -> LogDouble(0.1), "gato" -> LogDouble(0.1), "marche" -> LogDouble(0.1))),
//            "S"  -> new SimpleLogProbabilityDistribution(Map("the" -> LogDouble(0.1), "cat" -> LogDouble(0.1), "walks" -> LogDouble(0.1), "el" -> LogDouble(0.2), "gato" -> LogDouble(0.2), "marche" -> LogDouble(0.2))),
//            "xS" -> new SimpleLogProbabilityDistribution(Map("the" -> LogDouble(0.1), "cat" -> LogDouble(0.1), "walks" -> LogDouble(0.1), "el" -> LogDouble(0.2), "gato" -> LogDouble(0.2), "marche" -> LogDouble(0.2))),
//        ))
    
    // TODO: The UNK words appear multiple times......
//    val enWords  = (csTrainData ++ csEvalData).flatten.toSet[(String, String)].filter(wc => enWordCounts.contains(wc._1)).collect { case (word, lang) if lang == "E" || lang == "S" => word }
//    val esWords  = (csTrainData ++ csEvalData).flatten.toSet[(String, String)].filter(wc => esWordCounts.contains(wc._1)).collect { case (word, lang) if lang == "E" || lang == "S" => word }
////    val enXWords = (csTrainData ++ csEvalData).flatten.toSet[(String, String)].filter(wc => enWordCounts.contains(wc._1)).collect { case (word, lang) if lang != "E" && lang != "S" => word }
////    val esXWords = (csTrainData ++ csEvalData).flatten.toSet[(String, String)].filter(wc => esWordCounts.contains(wc._1)).collect { case (word, lang) if lang != "E" && lang != "S" => word }
//    val xWords   = (csTrainData ++ csEvalData).flatten.toSet[(String, String)].collect { case (word, lang) if lang != "E" && lang != "S" => word }
//    val unkWords = (csTrainData.flatten.map(_._1).counts.collect{ case (word, count) if count == 1 => word }.toSet ++ csEvalData.flatten.map(_._1))
//                       .filter(word => !enWords(word) && !esWords(word) && !xWords(word))
//    val xunkWords = (csTrainData.flatten.map(_._1).counts.collect{ case (word, count) if count == 1 => word }.toSet ++ csEvalData.flatten.map(_._1))
//                       .filter(word => !enWords(word) && !esWords(word) && xWords(word))
//    val noneOfTheAbove = (csTrainData ++ csEvalData).flatten.map(_._1).toSet[String].filter{ word => !enWords(word) && !esWords(word) && !xWords(word) && !unkWords(word) && !xunkWords(word) }
//    
//    for (w <- Vector("i")) println(s"$w :: enWords=${enWords(w)}, esWords=${esWords(w)}, xWords=${xWords(w)}, unkWords=${unkWords(w)}")
    
//    println(s"enWords : ${enWords.take(10).mkString(", ")}")
//    println(s"esWords : ${esWords.take(10).mkString(", ")}")
//    println(s"xWords : ${xWords.take(10).mkString(", ")}")
//    println(s"unkWords : ${unkWords.take(10).mkString(", ")}")
//    println(s"xunkWords : ${xunkWords.take(10).mkString(", ")}")
//    println(s"none of the above : ${noneOfTheAbove.take(10).mkString(", ")}")

    val tagdict = SimpleTagDictionary((csAllTypes.filter(isNonX) & monoEnOnlyTypes).mapToVal(Set("E")).toMap ++ 
                                      (csAllTypes.filter(isNonX) & monoEsOnlyTypes).mapToVal(Set("S")).toMap ++
                                      (csAllTypes.filter(isNonX) -- monoEnOnlyTypes -- monoEsOnlyTypes).mapToVal(Set("E", "S")).toMap ++
                                      csAllTypes.filter(isX).mapToVal(Set("xE", "xS")).toMap ++
                                      Map("<UNK>" -> Set("E","S"), "<xUNK>" -> Set("xE","xS")),
                                      "<S>", "<S>", "<E>", "<E>")

    def replaceUnk(word: String) = word match {
                                     case word if csUnkTypes(word) && isNonX(word) => "<UNK>"
                                     case word if csUnkTypes(word) && isX(word) => "<xUNK>"
                                     case word => word
                                   }
    def replaceUnks(sentence: Vector[(String, String)]): Vector[String] = sentence.map { case (word, lang) => replaceUnk(word) }
    def cleanLangLabel(word: String, lang: String): String = lang match {
      case _ if isX(word) => "x"
      case "E" | "S" => lang
      case _ => "x"
    }
                              	  
	  val trainInput = csTrainData.map(replaceUnks)
//    val trainInput = Vector(Vector("the", "cat", "walks"))
//    println(s"TRAIN INPUT:\n")
//    for (s <- Vector(Vector("omgggg")) ++ trainInput.take(10)) {
//      for (w <- s)
//        println(s"$w :: en=${enWords(w)}, es=${esWords(w)}, x=${xWords(w)}, unk=${unkWords(w)} neverX=${neverXWords(w)} :: alwaysX=${alwaysXWords(w)} :: ${tagdict(w).map(t => (t, initEmiss(w,t))) }")
//      println
//    }

	  val results =
  	  for (maxIter <- (0 to 4) ++ (5 to 20 by 5)) yield {  //(0 to 10) ++ (15 to 50 by 5)) yield {
        val emTrainer = new SoftEmHmmTaggerTrainer[Tag](
                                  	  maxIterations = maxIter,
                                  	  new UnsmoothedTransitionDistributioner, new UnsmoothedEmissionDistributioner,
                                  	  alphaT = 0.0, alphaE = 0.0, 1e-10)
        val emHmm = emTrainer.train(trainInput, tagdict, initTrans, initEmiss)
        val goldTags = Vector("<S>", "<E>") ++ csEvalData.flatten.map(_._2).sorted.distinct
        val goldTagIndex = goldTags.zipWithIndex.toMap
        val modelTags = Vector("<S>", "<E>") ++ tagdict.allTags.toVector.sorted
        val modelTagIndex = modelTags.zipWithIndex.toMap
        val confusionMatrix: Array[Array[Int]] = Array.fill(goldTagIndex.size)(Array.fill(modelTagIndex.size)(0))
        var numCorrect = 0
        var totalCount = 0
        for (sentence  <- csEvalData) {
          for (((word, goldLang), predictedLang) <- (sentence zipSafe emHmm.tag(replaceUnks(sentence)))) {
            confusionMatrix(goldTagIndex(goldLang))(modelTagIndex(predictedLang)) += 1
            val cleanGoldLabel = cleanLangLabel(word, goldLang)
            if (cleanGoldLabel != "x") {  // Just evaluate when the gold annotation is a language label.
              if (cleanGoldLabel == cleanLangLabel(word, predictedLang)) numCorrect += 1
          		  totalCount += 1
            }
          }
        }
        val acc = numCorrect / totalCount.toDouble
        println(f"acc = ($numCorrect/$totalCount) = ${acc}")
        
        for (srow <- Vector((Vector("") ++ modelTags.drop(2))) ++ (goldTags zipSafe confusionMatrix).drop(2).map { case (g, row) => Vector(g) ++ row.drop(2).map(_.toString) }) {
          println(srow.map(scell => f"$scell%10s").mkString(" "))
        }
        
        (maxIter, numCorrect / totalCount.toDouble)
      }
	  for ((maxIter, acc) <- results) {
	    println(f"$maxIter%3d  ${acc*100}%.2f")
	  }
  }
}

// maxIterations, accuracy
//  0  29.25
//  1  79.79
//  2  79.31
//  3  79.08
//  4  78.90
//  5  78.40
//  6  78.31
//  7  77.98
//  8  77.64
//  9  77.42
// 10  77.09
// 15  75.48
// 20  74.49
// 25  73.84
// 30  73.63
// 35  73.34
// 40  73.00
// 45  72.61
// 50  72.43

//acc = (5573/6985) = 0.7978525411596278
//                    E          S         xE         xS
//         E       1573       1231          0          0
//         S        181       4000          0          0
// ambiguous          1         14          0          0
//     mixed          0          7          0          0
//        ne         86        124          0          0
//     other         46        137        690       1197
//       unk          9         28          0          0
//
//acc = (5385/6985) = 0.7709377236936292
//                    E          S         xE         xS
//         E       1352       1452          0          0
//         S        148       4033          0          0
// ambiguous          1         14          0          0
//     mixed          0          7          0          0
//        ne         50        160          0          0
//     other         18        165        798       1089
//       unk          6         31          0          0
