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
import nlp.hmm.prob.InterpolatingConditionalLogProbabilityDistribution
import scalaz._
import Scalaz._
import nlp.hmm.prob.ConditionalLogProbabilityDistribution
import nlp.hmm.prob.MapLogProbabilityDistribution
import nlp.hmm.tag.MemmTaggerTrainer
import scala.collection.mutable.Buffer
import nlp.hmm.tag.HmmTagger
import nlp.hmm.prob.SimpleExpProbabilityDistribution

object Codemix {
  type Word = String
  type Tag = String
  
  val useMemm = false
  val useMonoForMemm = false
  val unkCutoffCount = 1

  val PuncOnlyRe = raw"[^A-Za-z0-9]+".r
  val AtMentionRe = raw"@.+".r
  def normalizeWord(word: String) = word.toLowerCase match {
//    case AtMentionRe() => "@mention"
    case w => w
  }
  
    def isX(word: String) = word match {
      case PuncOnlyRe() => true
//      case AtMentionRe() => true
      case _ => false
    }
    def isNonX(word: String) = !isX(word)

  def readConlluRawData(filenames: Vector[String], lang: String): Vector[Vector[(String, String)]] = {
    filenames.flatMap { filename =>
      File(filename).readLines.splitWhere(_.trim.isEmpty).map(_.flatMap { line =>
        if (line.trim.isEmpty) None
        else if (line.trim.startsWith("#")) None
        else {
          val word = normalizeWord(line.splitWhitespace(1))
          if (isNonX(word)) Some((word, lang))
          else Some((word, "x"+lang))
        }
      })
    }
  }
  
  def wordToChars(word: String, n: Int) = {
    Vector.fill(n - 1)("<S>") ++ word.map(_.toString) ++ Vector("<E>")
  }
  
  def wordToCharNgrams(word: String, n: Int) = {
    wordToChars(word, n).sliding(n).map { case context :+ c => (context, c) }.toVector
  }

  def wordCountsToCharNgramCounts(n: Int, wordCounts: Map[String, Int]): Map[Vector[String], Map[String, Int]] = {
    wordCounts.toVector
      .flatMap { case (word, wordCount) => wordToCharNgrams(word, n).map { case (context, head) => (context, (head, wordCount)) } }
      .groupByKey
      .mapVals(_.groupByKey.mapVals(_.sum))
  }
  
  def makeNgramModel(maxN: Int, wordCounts: Map[String, Int]) = {
    val models =
      (maxN downto 1).toVector.map { n =>
        val conditionalNgramCounts = wordCountsToCharNgramCounts(n, wordCounts)
        new SimpleConditionalLogProbabilityDistribution(
          conditionalNgramCounts.mapVals { ngramCounts =>
            new LaplaceLogProbabilityDistribution[String](
                ngramCounts.mapVals(LogDouble(_)), None, None, LogDouble(1), LogDouble(1))},
        new LaplaceLogProbabilityDistribution[String](
                conditionalNgramCounts.map(_._2).reduce(_ |+| _).mapVals(LogDouble(_)), None, None, LogDouble(1), LogDouble(1)))
      }
    val unnormalizedWeights = Vector.tabulate(maxN)(n => 1.0 / (0.5 ** n))
    val normalizedWeights = unnormalizedWeights.map(w => LogDouble(w / unnormalizedWeights.sum))
    new InterpolatingConditionalLogProbabilityDistribution(models zipSafe normalizedWeights)
  }
  
  def charSequenceProbability(word: String, n: Int, model: ConditionalLogProbabilityDistribution[Vector[String], String]) = {
    val ngrams = wordToCharNgrams(word, n)
    val product = ngrams.map { case (context, head) => model(head, context) }.product
    val perplexity = new LogDouble((LogDouble(1.0) / product).logValue * (1.0 / ngrams.size))
    val invertedPerplexity = (LogDouble(1) / perplexity)
    LogDouble(math.E ** invertedPerplexity.toDouble)  // When wrapped in the probability distribution, it will be: softmax(1/perplexity)
  }
  
  def readLangTaggedSplFile(filenames: Vector[String], numColumns: Int) = {
    filenames.flatMap { filename =>
      File(filename).readLines.map { line =>
        line.splitWhitespace.map(token => (numColumns, token.rsplit(raw"\|", numColumns)) match {
          case (2, Vector(word, "en")) => (normalizeWord(word), "E")
          case (2, Vector(word, "es")) => (normalizeWord(word), "S")
          case (2, Vector(word, lang)) => (normalizeWord(word), lang)
          case (3, Vector(word, "en", _)) => (normalizeWord(word), "E")
          case (3, Vector(word, "es", _)) => (normalizeWord(word), "S")
          case (3, Vector(word, lang, _)) => (normalizeWord(word), lang)
        })
      }
    }
  }
  
  def readRawSentences(filenames: Vector[String], lang: String): Vector[Vector[(String, String)]] = {
    filenames.flatMap { filename =>
      File(filename).readLines.map { line =>
        line.splitWhitespace.map { token =>
          val word = normalizeWord(token)
          if (isNonX(word)) (word, lang)
          else (word, "x"+lang)
        }
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

//    val monoEnData = readConlluRawData(Vector("data/ud/UD_English-EWT-master/en-ud-train.conllu"), "E")
//    //val monoEsData = readConlluRawData(Vector("data/ud/UD_Spanish-GSD-master/es-ud-train.conllu"), "S")// -- monoEnWordCounts.keys
//    val monoEsData = readConlluRawData(Vector("data/ud/UD_Hindi-HDTB-master/hi-ud-train.conllu"), "S")// -- monoEnWordCounts.keys
    val monoEnData = readRawSentences(Vector("data/ud/UD_English-EWT-master/train_latn.spl"), "E")
    val monoEsData = readRawSentences(Vector("data/ud/UD_Hindi-HDTB-master/train_latn.spl"), "S")// -- monoEnWordCounts.keys
    val monoEnWordCounts = monoEnData.flatten.map(_._1).counts
    val monoEsWordCounts = monoEsData.flatten.map(_._1).counts// -- monoEnWordCounts.keys
//    val csTrainData = readLangTaggedSplFile(Vector("data/wcacs16/en_es/train.spl"), 2)
//    val csEvalData = readLangTaggedSplFile(Vector("data/wcacs16/en_es/dev.spl"), 2)
    val csTrainData = readLangTaggedSplFile(Vector("data/irshad/en_hi/dev.spl",
                                                   "data/fire/en_hi/dev.spl",
                                                   "data/icon/en_hi/dev.spl",
                                                   "data/msr/en_hi/dev.spl"), 2)
    val csEvalData = readLangTaggedSplFile(Vector("data/irshad/en_hi/dev.spl"), 2)
    
    val perplexityCheckWords = Vector("acceleration", "dhg", "dhgg", "aardvark", "aardvarks", "why", "telephone", "who", "how", "when", "where", "what", "the", "a", "szozezizjzfzso")
    def checkPerplexities(charNgramOrder: Int, model: ConditionalLogProbabilityDistribution[Vector[String], String]) = {
      println("\ncheckPerplexities")
      val wordProbs = perplexityCheckWords.mapTo { w => charSequenceProbability(w, charNgramOrder, model) }.normalizeValues
      for ((w,p) <- wordProbs.sortBy(_._2).reverse) {
        println(f"$w%20s = ${p.toDouble}%25.8f")
      }
    }
    def checkPerplexities2(model: ConditionalLogProbabilityDistribution[String, String]) = {
      println("\ncheckPerplexities2")
      val wordPerplexities = perplexityCheckWords.mapTo { w => model(w, "E").toDouble }
      for ((w,p) <- wordPerplexities.sortBy(-_._2)) {
        println(f"$w%20s = $p%25.8f")
      }
    }
    
    val csAllTypes = (csTrainData ++ csEvalData).flatten.map(_._1).toSet
      val csCutoffTypes = csTrainData.flatten.map(_._1).counts.collect{ case (word, count) if count <= unkCutoffCount => word }.toVector//.take(2500)  // I don't know why this is necessary :-(
      println("csCutoffTypes.size() = " + csCutoffTypes.size)
    val csGoodTypes = csTrainData.flatten.map(_._1).toSet -- csCutoffTypes
    val monoEnOnlyTypes = monoEnWordCounts.keySet -- monoEsWordCounts.keySet
    val monoEsOnlyTypes = monoEnWordCounts.keySet -- monoEsWordCounts.keySet
    val csUnkTypes = csAllTypes -- csGoodTypes -- monoEnOnlyTypes -- monoEsOnlyTypes

    val initEmissUnigramWord = new SimpleConditionalLogProbabilityDistribution(
        Map[Tag, LogProbabilityDistribution[Word]](
            "<S>" -> new SimpleLogProbabilityDistribution(Map("<S>" -> LogDouble(1))),
            "<E>" -> new SimpleLogProbabilityDistribution(Map("<E>" -> LogDouble(1))),
            "E"  -> new LaplaceLogProbabilityDistribution(monoEnWordCounts.collect { case (word, count) if isNonX(word) => (word, LogDouble(count)) }, None, excludedBs = Some(Set("<S>", "<E>")), LogDouble(1.0), LogDouble(1.0)),
            "S"  -> new LaplaceLogProbabilityDistribution(monoEsWordCounts.collect { case (word, count) if isNonX(word) => (word, LogDouble(count)) }, None, excludedBs = Some(Set("<S>", "<E>")), LogDouble(1.0), LogDouble(1.0)),
            "xE"  -> new LaplaceLogProbabilityDistribution(monoEnWordCounts.collect { case (word, count) if isX(word) => (word, LogDouble(count)) }, None, excludedBs = Some(Set("<S>", "<E>")), LogDouble(1.0), LogDouble(1.0)),
            "xS"  -> new LaplaceLogProbabilityDistribution(monoEsWordCounts.collect { case (word, count) if isX(word) => (word, LogDouble(count)) }, None, excludedBs = Some(Set("<S>", "<E>")), LogDouble(1.0), LogDouble(1.0)),
        ))
    def initEmissCharNgrams(charNgramOrder: Int) = {
      val monoEnNgramModel = makeNgramModel(charNgramOrder, monoEnWordCounts)
      val monoEsNgramModel = makeNgramModel(charNgramOrder, monoEsWordCounts)
      checkPerplexities(charNgramOrder, monoEnNgramModel)
      val allTypes = csAllTypes ++ perplexityCheckWords
      val model =
        new SimpleConditionalLogProbabilityDistribution(
          Map[Tag, LogProbabilityDistribution[Word]](
              "<S>" -> new SimpleLogProbabilityDistribution(Map("<S>" -> LogDouble(1))),
              "<E>" -> new SimpleLogProbabilityDistribution(Map("<E>" -> LogDouble(1))),
              "E"  -> new MapLogProbabilityDistribution((allTypes.filter(isNonX).mapTo(word => charSequenceProbability(word, charNgramOrder, monoEnNgramModel)).toMap.normalizeValues ++ Set("<UNK>", "<xUNK>").mapToVal(LogDouble(1e-10))).withDefaultValue(LogDouble.zero)),// ++ Set("<S>", "<E>").mapToVal(LogDouble.zero)),
              "S"  -> new MapLogProbabilityDistribution((allTypes.filter(isNonX).mapTo(word => charSequenceProbability(word, charNgramOrder, monoEsNgramModel)).toMap.normalizeValues ++ Set("<UNK>", "<xUNK>").mapToVal(LogDouble(1e-10))).withDefaultValue(LogDouble.zero)),// ++ Set("<S>", "<E>").mapToVal(LogDouble.zero)),
              "xE"  -> new MapLogProbabilityDistribution((allTypes.filter(isX).mapTo(word => charSequenceProbability(word, charNgramOrder, monoEnNgramModel)).toMap.normalizeValues ++ Set("<UNK>", "<xUNK>").mapToVal(LogDouble(1e-10))).withDefaultValue(LogDouble.zero)),// ++ Set("<S>", "<E>").mapToVal(LogDouble.zero)),
              "xS"  -> new MapLogProbabilityDistribution((allTypes.filter(isX).mapTo(word => charSequenceProbability(word, charNgramOrder, monoEsNgramModel)).toMap.normalizeValues ++ Set("<UNK>", "<xUNK>").mapToVal(LogDouble(1e-10))).withDefaultValue(LogDouble.zero)),// ++ Set("<S>", "<E>").mapToVal(LogDouble.zero)),
//              "E"  -> new MapLogProbabilityDistribution((allTypes.filter(isNonX).mapTo(word => LogDouble(1e-5)).toMap ++ Set("<UNK>", "<xUNK>").mapToVal(LogDouble(1e-10))).withDefaultValue(LogDouble.zero)),// ++ Set("<S>", "<E>").mapToVal(LogDouble.zero)),
//              "S"  -> new MapLogProbabilityDistribution((allTypes.filter(isNonX).mapTo(word => LogDouble(1e-5)).toMap ++ Set("<UNK>", "<xUNK>").mapToVal(LogDouble(1e-10))).withDefaultValue(LogDouble.zero)),// ++ Set("<S>", "<E>").mapToVal(LogDouble.zero)),
//              "xE"  -> new MapLogProbabilityDistribution((allTypes.filter(isX).mapTo(word => LogDouble(1e-5)).toMap ++ Set("<UNK>", "<xUNK>").mapToVal(LogDouble(1e-10))).withDefaultValue(LogDouble.zero)),// ++ Set("<S>", "<E>").mapToVal(LogDouble.zero)),
//              "xS"  -> new MapLogProbabilityDistribution((allTypes.filter(isX).mapTo(word => LogDouble(1e-5)).toMap ++ Set("<UNK>", "<xUNK>").mapToVal(LogDouble(1e-10))).withDefaultValue(LogDouble.zero)),// ++ Set("<S>", "<E>").mapToVal(LogDouble.zero)),
          ))
      checkPerplexities2(model)
      model
    }
    def initEmissInterpUnigramWordCharNgrams(initEmissInterpUnigramWeight: Double, charNgramOrder: Int) = {
      new InterpolatingConditionalLogProbabilityDistribution(Vector(
        (initEmissUnigramWord, LogDouble(initEmissInterpUnigramWeight)),
        (initEmissCharNgrams(charNgramOrder), LogDouble(1 - initEmissInterpUnigramWeight))))
    }
    val initEmiss = initEmissInterpUnigramWordCharNgrams(0.75, 3)
      checkPerplexities2(initEmiss)
    
//    val initEmiss = new SimpleConditionalLogProbabilityDistribution(
//        Map[Tag, LogProbabilityDistribution[Word]](
//            "<S>" -> new SimpleLogProbabilityDistribution(Map("<S>" -> LogDouble(1))),
//            "<E>" -> new SimpleLogProbabilityDistribution(Map("<E>" -> LogDouble(1))),
//            "E"  -> new SimpleLogProbabilityDistribution(Map("the" -> LogDouble(0.2), "cat" -> LogDouble(0.2), "walks" -> LogDouble(0.2), "el" -> LogDouble(0.1), "gato" -> LogDouble(0.1), "marche" -> LogDouble(0.1))),
//            "xE" -> new SimpleLogProbabilityDistribution(Map("the" -> LogDouble(0.2), "cat" -> LogDouble(0.2), "walks" -> LogDouble(0.2), "el" -> LogDouble(0.1), "gato" -> LogDouble(0.1), "marche" -> LogDouble(0.1))),
//            "S"  -> new SimpleLogProbabilityDistribution(Map("the" -> LogDouble(0.1), "cat" -> LogDouble(0.1), "walks" -> LogDouble(0.1), "el" -> LogDouble(0.2), "gato" -> LogDouble(0.2), "marche" -> LogDouble(0.2))),
//            "xS" -> new SimpleLogProbabilityDistribution(Map("the" -> LogDouble(0.1), "cat" -> LogDouble(0.1), "walks" -> LogDouble(0.1), "el" -> LogDouble(0.2), "gato" -> LogDouble(0.2), "marche" -> LogDouble(0.2))),
//        ))
    
    val tagdict = SimpleTagDictionary(
//        (csAllTypes.filter(isNonX) & monoEnOnlyTypes).mapToVal(Set("E")).toMap ++ 
//        (csAllTypes.filter(isNonX) & monoEsOnlyTypes).mapToVal(Set("S")).toMap ++
//        (csAllTypes.filter(isNonX) -- monoEnOnlyTypes -- monoEsOnlyTypes).mapToVal(Set("E", "S")).toMap ++
        (csAllTypes ++ monoEnWordCounts.keySet ++ monoEsWordCounts.keySet).filter(isNonX).mapToVal(Set("E", "S")).toMap ++
        (csAllTypes ++ monoEnWordCounts.keySet ++ monoEsWordCounts.keySet).filter(isX).mapToVal(Set("xE", "xS")).toMap ++
        Map("<UNK>" -> Set("E","S"), "<xUNK>" -> Set("xE","xS")),
                              "<S>", "<S>", "<E>", "<E>")

    // TODO: Can we set things up to always use the char-ngram prob instead of using UNK?
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

      val trainInput = csTrainData.map(_.map(_._1))
        
    for (s <- trainInput.drop(20).take(2)) { println; for (w <- s) println(f"${replaceUnk(w)}%20s  [${tagdict(w).mkString(", ")}]") }
  
    val results: IndexedSeq[(Int, Double)] =
        for (maxIter <- (1000 to 1000)) yield {//(0 to 4) ++ (5 to 20 by 5) ++ (30 to 30 by 10)) yield {// ++ (30 to 100 by 10)) yield {  //(0 to 10) ++ (15 to 50 by 5)) yield {
          val emTrainer = new SoftEmHmmTaggerTrainer[Tag](
                                        maxIterations = maxIter,
                                        new UnsmoothedTransitionDistributioner, new UnsmoothedEmissionDistributioner,
                                        alphaT = 0.0, alphaE = 0.0, 1e-10)
          val supervisedMemmTrainer = new MemmTaggerTrainer[Tag](maxIterations = 100, cutoff = 100, tdRestricted = true, identity, identity)
          
          val emHmm = emTrainer.train(trainInput.map(_.map(replaceUnk)), tagdict, initTrans, initEmiss)
          val memm = if (useMemm) supervisedMemmTrainer.train(
                  trainInput.map(sentence => sentence.zipSafe(emHmm.tag(sentence.map(replaceUnk)))) ++
                    (if (useMonoForMemm) monoEnData ++ monoEsData else Vector.empty), // Also train MEMM on monolingual data
                  tagdict)
              else null
              
          {
            for (csUnsupFile <- Vector("fire", "icon", "irshad", "msr")) {
              val csUnsupData = readLangTaggedSplFile(Vector(s"data/$csUnsupFile/en_hi/dev.spl"), 2)
              val allWords = csEvalData.flatMap(_.map(_._1)).toSet
              val wordTranslits: Map[String, Vector[SimpleExpProbabilityDistribution[String]]] = 
                  File("data/translit/latn2deva.txt").readLines
                      .map(_.splitWhitespace).collect { 
                        case Seq(w, translitsString) if allWords(w) => w -> Vector(
                            new SimpleExpProbabilityDistribution(Map(translitsString.lsplit(":", 2).head -> 1.0)),
                            new SimpleExpProbabilityDistribution(translitsString.lsplit(",").map(_.lsplit(":")).map { case Seq(v,p) => (v,p.toDouble) }.toMap))
                      }.toMap
              for (pd <- Seq(0,1)) {
                writeUsing(File(f"data/$csUnsupFile/en_hi/dev_unsup_labels_${pd match { case 0 => "onebest"; case 1 => "sampled" }}.spl")) { f =>  // unsup_dist_tagged_${maxIter}%03d.out")) { f =>
                  for (sentence <- csEvalData) {
                    val tagProbs = emHmm.asInstanceOf[HmmTagger[Tag]].tagToProbDistsFromTagSet(sentence.map { case (w,t) => replaceUnk(w) -> tagdict(w) })
                    f.writeLine(sentence.zipSafe(tagProbs).map {
                      case ((word, goldLang), modelOutputLangDist) =>
                        val filteredProbs = modelOutputLangDist.mapVals(_.toDouble).filter(_._2 >= 0.001).normalizeValues.toVector.sortBy(-_._2);
                        val goldTag = goldLang match {
                                case "E" => "en"
                                case "xE" => "en"
                                case "S" => "hi"
                                case "xS" => "hi"
                                case t => t
                        }
                        val tagDistString = filteredProbs.map { case (t,p) => f"${t match {
                                case "E" => "en"
                                case "xE" => "en"
                                case "S" => "hi"
                                case "xS" => "hi"
                            }}:${p}%.3f" }.mkString(",")
                        f"$word|$goldTag|$tagDistString|${wordTranslits.get(word).map(_(pd).sample()).getOrElse(word)}"
                    }.mkString(" "))
                  }
                }
              }
            }
          }

          def doEvaluate(evalData: Vector[Vector[(String, String)]], outputFilePrefix: String, outputTagDistributions: Boolean = false) = {
            val goldTags = Vector("<S>", "<E>") ++ evalData.flatten.map(_._2).sorted.distinct
            val goldTagIndex = goldTags.zipWithIndex.toMap
            val modelTags = Vector("<S>", "<E>") ++ tagdict.allTags.toVector.sorted
            val modelTagIndex = modelTags.zipWithIndex.toMap
            val confusionMatrix: Array[Array[Int]] = Array.fill(goldTagIndex.size)(Array.fill(modelTagIndex.size)(0))
            var numCorrect = 0
            var totalCount = 0
            writeUsing(File(f"output/${outputFilePrefix}_${maxIter}%03d.out")) { f =>
              for (sentence  <- evalData) {
                val predictedTags = {
                  if (outputTagDistributions) {
                    assert(!useMemm, "not supported yet")
                    emHmm.tag(replaceUnks(sentence))
                  } else {
                    if (useMemm) memm.tag(sentence.map(_._1))
                    else emHmm.tag(replaceUnks(sentence))
                  }
                }
                for (((word, goldLang), predictedLang) <- (sentence zipSafe predictedTags)) {
                  confusionMatrix(goldTagIndex(goldLang))(modelTagIndex(predictedLang)) += 1
                  val cleanGoldLabel = cleanLangLabel(word, goldLang)
                  if (cleanGoldLabel != "x") {  // Just evaluate when the gold annotation is a language label.
                    if (cleanGoldLabel == cleanLangLabel(word, predictedLang)) numCorrect += 1
                      totalCount += 1
                  }
                }
                f.writeLine(sentence.zipSafe(replaceUnks(sentence)).zipSafe(predictedTags).map { case (((word, gold), modifiedWord), predicted) => f"$word|$modifiedWord|$predicted|$gold" }.mkString(" "))
              }
            }
            val acc = numCorrect / totalCount.toDouble
            println(f"acc = ($numCorrect/$totalCount) = ${acc}")
            
            for (srow <- Vector((Vector("") ++ modelTags.drop(2))) ++ (goldTags zipSafe confusionMatrix).drop(2).map { case (g, row) => Vector(g) ++ row.drop(2).map(_.toString) }) {
              println(srow.map(scell => f"$scell%10s").mkString(" "))
            }
      
            // Find differences in HMM vs MEMM output
            if (useMemm) {
              for (sentence  <- evalData) {
                val hmmPredictedTags = emHmm.tag(replaceUnks(sentence))
                val memmPredictedTags = memm.tag(sentence.map(_._1))
                val outputLineParts = Buffer[String]()
                var containsMismatch = false
                for (((word, goldLang), (hmmPredictedLang, memmPredictedLang)) <- (sentence zipSafe (hmmPredictedTags zipSafe memmPredictedTags))) {
                  var str = f"$word|$goldLang|$hmmPredictedLang|$memmPredictedLang"
                  if (cleanLangLabel(word, hmmPredictedLang) != cleanLangLabel(word, memmPredictedLang)) {
                    containsMismatch = true
                    str = f"**$str**"
                  }
                  outputLineParts += str
                }
                if (containsMismatch) {
                  println(outputLineParts.mkString(" "))
                }
              }
            }
            
            (maxIter, numCorrect / totalCount.toDouble)
          }
          
          doEvaluate(csTrainData, "train", outputTagDistributions = false)
          doEvaluate(csEvalData, "eval", outputTagDistributions = false)
        }
    for ((maxIter, acc) <- results) {
      println(f"$maxIter%3d  ${acc*100}%.2f")
    }
  }
}
