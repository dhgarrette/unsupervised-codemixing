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
          case Vector(word, "en") => (word, "E")
          case Vector(word, "es") => (word, "S")
          case Vector(word, lang) => (word, lang)
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

    val enWordCounts = countConlluRawWords(Vector("data/ud/UD_English-EWT-master/en-ud-train.conllu"))
    val esWordCounts = countConlluRawWords(Vector("data/ud/UD_Spanish-GSD-master/es-ud-train.conllu"))
    val csTrainData = readLangTaggedSplFile(Vector("data/wcacs16/en_es/train.spl"))
    val csEvalData = readLangTaggedSplFile(Vector("data/wcacs16/en_es/dev.spl"))
    val csTrainRaw = csTrainData.map(_.map(_._1))
    val neverXWords  = (csTrainData ++ csEvalData).flatten.map { case (word, lang) => (word, lang != "E" && lang != "S") }.toSet[(String,Boolean)].groupByKey.collect { case (word, Coll(false)) => word }.toSet
    val alwaysXWords = (csTrainData ++ csEvalData).flatten.map { case (word, lang) => (word, lang != "E" && lang != "S") }.toSet[(String,Boolean)].groupByKey.collect { case (word, Coll(true)) => word }.toSet
    
    for (w <- (csTrainData ++ csEvalData).flatten.map { case (word, lang) => (word, lang != "E" && lang != "S") }.groupByKey.mapValues(_.counts).collect { case (word, valueCounts) if valueCounts.size > 1 => (word, valueCounts(true), valueCounts(false)) }.toVector.sorted ) println(s"ambiguous: $w")

    val initEmiss = new SimpleConditionalLogProbabilityDistribution(
        Map[Tag, LogProbabilityDistribution[Word]](
            "<S>" -> new SimpleLogProbabilityDistribution(Map("<S>" -> LogDouble(1))),
            "<E>" -> new SimpleLogProbabilityDistribution(Map("<E>" -> LogDouble(1))),
            "E"  -> new LaplaceLogProbabilityDistribution(enWordCounts.mapVals(LogDouble(_)), None, excludedBs = Some(alwaysXWords + ("<S>", "<E>")), LogDouble(1.0), LogDouble(1.0)),
            "S"  -> new LaplaceLogProbabilityDistribution(esWordCounts.mapVals(LogDouble(_)), None, excludedBs = Some(alwaysXWords + ("<S>", "<E>")), LogDouble(1.0), LogDouble(1.0)),
            "xE" -> new LaplaceLogProbabilityDistribution(enWordCounts.mapVals(LogDouble(_)), None, excludedBs = Some(neverXWords + ("<S>", "<E>")), LogDouble(1.0), LogDouble(1.0)),
            "xS" -> new LaplaceLogProbabilityDistribution(esWordCounts.mapVals(LogDouble(_)), None, excludedBs = Some(neverXWords + ("<S>", "<E>")), LogDouble(1.0), LogDouble(1.0)),
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
    
    val emTrainer = new SoftEmHmmTaggerTrainer[Tag](
        maxIterations = 10,
        new UnsmoothedTransitionDistributioner, new UnsmoothedEmissionDistributioner,
        alphaT = 0.0, alphaE = 0.0, 1e-10)
    val tagdict = SimpleTagDictionary(Map(), "<S>", "<S>", "<E>", "<E>")
                      .withWords(csTrainRaw.flatten.toSet)
                      .withTags(Set("E", "xE", "S", "xS"))

    val trainInput = csTrainRaw
//    val trainInput = Vector(Vector("the", "cat", "walks"))
    println(s"TRAIN INPUT:")
    for (w <- trainInput.head)
      println(s"$w :: never=${neverXWords(w)} :: always=${alwaysXWords(w)} :: ${tagdict(w).map(t => (t, initEmiss(w,t))) }")
    val emHmm = emTrainer.train(trainInput, tagdict, initTrans, initEmiss)
    val goldTags = Vector("<S>", "<E>") ++ csEvalData.flatten.map(_._2).sorted.distinct
    val goldTagIndex = goldTags.zipWithIndex.toMap
    val modelTags = Vector("<S>", "<E>") ++ tagdict.allTags.toVector.sorted
    val modelTagIndex = modelTags.zipWithIndex.toMap
    val confusionMatrix: Array[Array[Int]] = Array.fill(goldTagIndex.size)(Array.fill(modelTagIndex.size)(0))
    var numCorrect = 0
    var totalCount = 0
    for (sentence  <- csEvalData) {
      for (((word, goldLang), predictedLang) <- (sentence zipSafe emHmm.tag(sentence.map(_._1)))) {
        confusionMatrix(goldTagIndex(goldLang))(modelTagIndex(predictedLang)) += 1
        val simplifiedGoldLang      =      goldLang match { case "E" => "E"; case "S" => "S"; case _ => "x" }
        val simplifiedPredictedLang = predictedLang match { case "E" => "E"; case "S" => "S"; case _ => "x"  }
        if (goldLang == predictedLang) numCorrect += 1
        totalCount += 1
      }
    }
    val numTotal = csEvalData.flatten.size
    println(f"acc = ($numCorrect/$totalCount) = ${numCorrect / totalCount.toDouble}")
    
    for (srow <- Vector((Vector("") ++ modelTags.drop(2))) ++ (goldTags zipSafe confusionMatrix).drop(2).map { case (g, row) => Vector(g) ++ row.drop(2).map(_.toString) }) {
      println(srow.map(scell => f"$scell%10s").mkString(" "))
    }
  }
}
