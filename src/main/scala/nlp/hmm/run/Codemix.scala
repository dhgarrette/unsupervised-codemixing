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
          val word = normalizeWord(line.splitWhitespace.apply(1))
          if (isNonX(word)) Some((word, lang))
          else Some((word, "x"+lang))
        }
      })
    }
  }
  
  def wordToChars(word: String, n: Int): Vector[String] = {
    Vector.fill(n - 1)("<S>") ++ word.map(_.toString.toLowerCase) ++ Vector("<E>")
  }
  
  def wordToCharNgrams(word: String, n: Int): Vector[(Vector[String], String)] = {
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
          case (2, Vector(word, "en")) => (normalizeWord(word), "E", word)
          case (2, Vector(word, "es")) => (normalizeWord(word), "H", word)
          case (2, Vector(word, lang)) => (normalizeWord(word), lang, word)
          case (3, Vector(word, "en", _)) => (normalizeWord(word), "E", word)
          case (3, Vector(word, "es", _)) => (normalizeWord(word), "H", word)
          case (3, Vector(word, lang, _)) => (normalizeWord(word), lang, word)
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
    // Tags:  <S>, <E>, E, xE, S, xH 
    val initTrans = new SimpleConditionalLogProbabilityDistribution(
        Map[Tag, LogProbabilityDistribution[Tag]](
            "<S>" -> new SimpleLogProbabilityDistribution(Map(
                "E"  -> LogDouble(0.25),
                "xE" -> LogDouble(0.25),
                "H"  -> LogDouble(0.25),
                "xH" -> LogDouble(0.25))),
            "<E>" -> new SimpleLogProbabilityDistribution(Map()),
            "E" -> new SimpleLogProbabilityDistribution(Map(
                "E"  -> LogDouble(0.65),
                "xE" -> LogDouble(0.24),
                "H"  -> LogDouble(0.01),
                "<E>" -> LogDouble(0.10))),
            "xE" -> new SimpleLogProbabilityDistribution(Map(
                "E"  -> LogDouble(0.80),
                "xE" -> LogDouble(0.15),
                "H"  -> LogDouble(0.05),
                "<E>" -> LogDouble(0.10))),
            "H" -> new SimpleLogProbabilityDistribution(Map(
                "E"  -> LogDouble(0.01),
                "H"  -> LogDouble(0.65),
                "xH" -> LogDouble(0.24),
                "<E>" -> LogDouble(0.10))),
            "xH" -> new SimpleLogProbabilityDistribution(Map(
                "E"  -> LogDouble(0.05),
                "H"  -> LogDouble(0.80),
                "xH" -> LogDouble(0.15),
                "<E>" -> LogDouble(0.10))),
            ))

    val evalset = "dev"
//    val monoEnData = readConlluRawData(Vector("data/ud/UD_English-EWT-master/en-ud-train.conllu"), "E")
//    //val monoHiData = readConlluRawData(Vector("data/ud/UD_Spanish-GSD-master/es-ud-train.conllu"), "H")// -- monoEnWordCounts.keys
//    val monoHiData = readConlluRawData(Vector("data/ud/UD_Hindi-HDTB-master/hi-ud-train.conllu"), "H")// -- monoEnWordCounts.keys
    val monoEnData = readRawSentences(Vector("data/ud/UD_English-EWT-master/train_latn.spl"), "E")
    val monoHiData = readRawSentences(Vector("data/ud/UD_Hindi-HDTB-master/train_latn.spl"), "H")// -- monoEnWordCounts.keys
    val monoEnWordCounts = monoEnData.flatten.map(_._1).counts
    val monoHiWordCounts = monoHiData.flatten.map(_._1).counts// -- monoEnWordCounts.keys
//    val csTrainData = readLangTaggedSplFile(Vector("data/wcacs16/en_es/train.spl"), 2)
//    val csEvalData = readLangTaggedSplFile(Vector("data/wcacs16/en_es/$evalset.spl"), 2)
    val csTrainData = readLangTaggedSplFile(Vector("data/irshad/en_hi/dev.spl",
                                                   "data/fire/en_hi/dev.spl",
                                                   "data/icon/en_hi/dev.spl",
                                                   "data/msr/en_hi/dev.spl") ++ (
                                                       if (evalset=="test") 
                                                         Vector("data/irshad/en_hi/test.spl",
                                                                "data/fire/en_hi/test.spl",
                                                                "data/icon/en_hi/test.spl",
                                                                "data/msr/en_hi/test.spl")
                                                       else Vector.empty), 2)
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
    val monoEnOnlyTypes = monoEnWordCounts.keySet -- monoHiWordCounts.keySet
    val monoHiOnlyTypes = monoEnWordCounts.keySet -- monoHiWordCounts.keySet
    val csUnkTypes = csAllTypes -- csGoodTypes -- monoEnOnlyTypes -- monoHiOnlyTypes

    val initEmissUnigramWord = new SimpleConditionalLogProbabilityDistribution(
        Map[Tag, LogProbabilityDistribution[Word]](
            "<S>" -> new SimpleLogProbabilityDistribution(Map("<S>" -> LogDouble(1))),
            "<E>" -> new SimpleLogProbabilityDistribution(Map("<E>" -> LogDouble(1))),
            "E"  -> new LaplaceLogProbabilityDistribution(monoEnWordCounts.collect { case (word, count) if isNonX(word) => (word, LogDouble(count)) }, None, excludedBs = Some(Set("<S>", "<E>")), LogDouble(1.0), LogDouble(1.0)),
            "H"  -> new LaplaceLogProbabilityDistribution(monoHiWordCounts.collect { case (word, count) if isNonX(word) => (word, LogDouble(count)) }, None, excludedBs = Some(Set("<S>", "<E>")), LogDouble(1.0), LogDouble(1.0)),
            "xE"  -> new LaplaceLogProbabilityDistribution(monoEnWordCounts.collect { case (word, count) if isX(word) => (word, LogDouble(count)) }, None, excludedBs = Some(Set("<S>", "<E>")), LogDouble(1.0), LogDouble(1.0)),
            "xH"  -> new LaplaceLogProbabilityDistribution(monoHiWordCounts.collect { case (word, count) if isX(word) => (word, LogDouble(count)) }, None, excludedBs = Some(Set("<S>", "<E>")), LogDouble(1.0), LogDouble(1.0)),
        ))
    def initEmissCharNgrams(charNgramOrder: Int) = {
      val monoEnNgramModel = makeNgramModel(charNgramOrder, monoEnWordCounts)
      val monoHiNgramModel = makeNgramModel(charNgramOrder, monoHiWordCounts)
      checkPerplexities(charNgramOrder, monoEnNgramModel)
      val allTypes = csAllTypes ++ perplexityCheckWords
      val model =
        new SimpleConditionalLogProbabilityDistribution(
          Map[Tag, LogProbabilityDistribution[Word]](
              "<S>" -> new SimpleLogProbabilityDistribution(Map("<S>" -> LogDouble(1))),
              "<E>" -> new SimpleLogProbabilityDistribution(Map("<E>" -> LogDouble(1))),
              "E"  -> new MapLogProbabilityDistribution((allTypes.filter(isNonX).mapTo(word => charSequenceProbability(word, charNgramOrder, monoEnNgramModel)).toMap.normalizeValues ++ Set("<UNK>", "<xUNK>").mapToVal(LogDouble(1e-10))).withDefaultValue(LogDouble.zero)),// ++ Set("<S>", "<E>").mapToVal(LogDouble.zero)),
              "H"  -> new MapLogProbabilityDistribution((allTypes.filter(isNonX).mapTo(word => charSequenceProbability(word, charNgramOrder, monoHiNgramModel)).toMap.normalizeValues ++ Set("<UNK>", "<xUNK>").mapToVal(LogDouble(1e-10))).withDefaultValue(LogDouble.zero)),// ++ Set("<S>", "<E>").mapToVal(LogDouble.zero)),
              "xE"  -> new MapLogProbabilityDistribution((allTypes.filter(isX).mapTo(word => charSequenceProbability(word, charNgramOrder, monoEnNgramModel)).toMap.normalizeValues ++ Set("<UNK>", "<xUNK>").mapToVal(LogDouble(1e-10))).withDefaultValue(LogDouble.zero)),// ++ Set("<S>", "<E>").mapToVal(LogDouble.zero)),
              "xH"  -> new MapLogProbabilityDistribution((allTypes.filter(isX).mapTo(word => charSequenceProbability(word, charNgramOrder, monoHiNgramModel)).toMap.normalizeValues ++ Set("<UNK>", "<xUNK>").mapToVal(LogDouble(1e-10))).withDefaultValue(LogDouble.zero)),// ++ Set("<S>", "<E>").mapToVal(LogDouble.zero)),
//              "E"  -> new MapLogProbabilityDistribution((allTypes.filter(isNonX).mapTo(word => LogDouble(1e-5)).toMap ++ Set("<UNK>", "<xUNK>").mapToVal(LogDouble(1e-10))).withDefaultValue(LogDouble.zero)),// ++ Set("<S>", "<E>").mapToVal(LogDouble.zero)),
//              "H"  -> new MapLogProbabilityDistribution((allTypes.filter(isNonX).mapTo(word => LogDouble(1e-5)).toMap ++ Set("<UNK>", "<xUNK>").mapToVal(LogDouble(1e-10))).withDefaultValue(LogDouble.zero)),// ++ Set("<S>", "<E>").mapToVal(LogDouble.zero)),
//              "xE"  -> new MapLogProbabilityDistribution((allTypes.filter(isX).mapTo(word => LogDouble(1e-5)).toMap ++ Set("<UNK>", "<xUNK>").mapToVal(LogDouble(1e-10))).withDefaultValue(LogDouble.zero)),// ++ Set("<S>", "<E>").mapToVal(LogDouble.zero)),
//              "xH"  -> new MapLogProbabilityDistribution((allTypes.filter(isX).mapTo(word => LogDouble(1e-5)).toMap ++ Set("<UNK>", "<xUNK>").mapToVal(LogDouble(1e-10))).withDefaultValue(LogDouble.zero)),// ++ Set("<S>", "<E>").mapToVal(LogDouble.zero)),
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
//            "H"  -> new SimpleLogProbabilityDistribution(Map("the" -> LogDouble(0.1), "cat" -> LogDouble(0.1), "walks" -> LogDouble(0.1), "el" -> LogDouble(0.2), "gato" -> LogDouble(0.2), "marche" -> LogDouble(0.2))),
//            "xH" -> new SimpleLogProbabilityDistribution(Map("the" -> LogDouble(0.1), "cat" -> LogDouble(0.1), "walks" -> LogDouble(0.1), "el" -> LogDouble(0.2), "gato" -> LogDouble(0.2), "marche" -> LogDouble(0.2))),
//        ))
    
    val tagdict = SimpleTagDictionary(
//        (csAllTypes.filter(isNonX) & monoEnOnlyTypes).mapToVal(Set("E")).toMap ++ 
//        (csAllTypes.filter(isNonX) & monoHiOnlyTypes).mapToVal(Set("H")).toMap ++
//        (csAllTypes.filter(isNonX) -- monoEnOnlyTypes -- monoHiOnlyTypes).mapToVal(Set("E", "H")).toMap ++
        (csAllTypes ++ monoEnWordCounts.keySet ++ monoHiWordCounts.keySet).filter(isNonX).mapToVal(Set("E", "H")).toMap ++
        (csAllTypes ++ monoEnWordCounts.keySet ++ monoHiWordCounts.keySet).filter(isX).mapToVal(Set("xE", "xH")).toMap ++
        Map("<UNK>" -> Set("E","H"), "<xUNK>" -> Set("xE","xH")),
                              "<S>", "<S>", "<E>", "<E>")

    // TODO: Can we set things up to always use the char-ngram prob instead of using UNK?
    def replaceUnk(word: String) = word match {
                                     case word if csUnkTypes(word) && isNonX(word) => "<UNK>"
                                     case word if csUnkTypes(word) && isX(word) => "<xUNK>"
                                     case word => word
                                   }
    def replaceUnks(sentence: Vector[(String, String, String)]): Vector[String] = sentence.map { case (word, lang, originalWord) => replaceUnk(word) }
    def cleanLangLabel(word: String, lang: String): String = lang match {
      case _ if isX(word) => "x"
      case "E" | "H" => lang
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
                    (if (useMonoForMemm) monoEnData ++ monoHiData else Vector.empty), // Also train MEMM on monolingual data
                  tagdict)
              else null
              
          {
            for (dataset <- Vector(/*"fire",*/ "icon", "irshad", "msr")) {
              val hiKnownEmbWords: Set[String] = File("data/emb/emb-polyglot-hi.txt").readLines.map(_.splitWhitespace.head).toSet
              val csPos = File(s"/Users/dhgarrette/workspace/unsupervised-codemixing/data/${dataset}/en_hi/tagged-${dataset}-hi_en-orig-coarse-${evalset}.txt").readLines.map(_.splitWhitespace.map(_.rsplit("\\|",3).toTuple3)).toVector
              val allWords = csPos.flatMap(_.map(_._1.toLowerCase)).toSet
              val wordTranslits: Map[String, Map[Boolean, Map[Boolean, SimpleExpProbabilityDistribution[String]]]] = 
                  File("data/translit/latn2deva.txt").readLines
                      .map(_.splitWhitespace).collect {
                        case Seq(w, translitsString) if allWords(w.toLowerCase) => w -> {
                          val translitsAndProbs: Vector[(String, Double)] = translitsString.lsplit(",").map(_.lsplit(":")).map { case Seq(v,p) => (v,p.toDouble) }
                          val allOnebest = new SimpleExpProbabilityDistribution(Map(translitsAndProbs.head._1 -> 1.0))
                          val allSampled = new SimpleExpProbabilityDistribution(translitsAndProbs.toMap)
                          Map(  // Maps preferKnownTranslit -> onebest -> probDist
                            false -> 
                              Map(true -> allOnebest, false -> allSampled),
                            true -> {
                              val knownTranslitsAndProbs = translitsAndProbs.filter { case (w,p) => hiKnownEmbWords(w) }
                              if (knownTranslitsAndProbs.nonEmpty) {
                                val knownOnebest = new SimpleExpProbabilityDistribution(Map(knownTranslitsAndProbs.head._1 -> 1.0))
                                val knownSampled = new SimpleExpProbabilityDistribution(knownTranslitsAndProbs.toMap)
                                Map(true -> knownOnebest, false -> knownSampled)
                              } else {
                                // None of the transliterations have known embeddings.
                                Map(true -> allOnebest, false -> allSampled)
                              }
                            })
                        }
                      }.toMap
              for (onebest <- Seq(false, true)) {
                for (preferKnownTranslit <- Seq(false, true)) {
                	  for (oracle <- Seq(false, true)) {
                    writeUsing(File(f"data/$dataset/en_hi/${evalset}_unsup_labels_${if (onebest) "onebest" else "sampled"}${if (preferKnownTranslit) "_known" else ""}${if (oracle) "_oracle" else ""}.spl")) { splOut =>  // unsup_dist_tagged_${maxIter}%03d.out")) { f =>
                    writeUsing(File(f"data/$dataset/en_hi/${evalset}-unsup-${if (onebest) "onebest" else "sampled"}${if (preferKnownTranslit) "_known" else ""}${if (oracle) "_oracle" else ""}.conllu")) { conllOut =>  // unsup_dist_tagged_${maxIter}%03d.out")) { f =>
                    writeUsing(File(f"data/$dataset/en_hi/${evalset}-oracle-unsup-${if (onebest) "onebest" else "sampled"}${if (preferKnownTranslit) "_known" else ""}${if (oracle) "_oracle" else ""}.conllu")) { conllOracleOut =>  // unsup_dist_tagged_${maxIter}%03d.out")) { f =>
                      for (sentence <- csPos) {
                        val tagProbs = emHmm.asInstanceOf[HmmTagger[Tag]].tagToProbDistsFromTagSet(sentence.map {
                          case (originalWord, goldLang, goldPos) => replaceUnk(normalizeWord(originalWord)) -> tagdict(normalizeWord(originalWord))
                        })
                        splOut.writeLine(sentence.zipSafe(tagProbs).zipWithIndex.map {
                          case (((originalWord, goldLang, goldPos), modelOutputLangDist), i) =>
                            val filteredProbs = modelOutputLangDist.mapVals(_.toDouble).filter(_._2 >= 0.001).normalizeValues.toVector.sortBy(-_._2);
                            val goldTag = goldLang match {
                                    case "E" => "en"
                                    case "xE" => "en"
                                    case "H" => "hi"
                                    case "xH" => "hi"
                                    case t => t
                            }
                            val tagDistString = 
                              if (oracle)
                                f"${goldTag match { case "hi" => "hi"; case _ => "en" }}:1"
                              else
                                filteredProbs.map { case (t,p) => f"${t match {
                                      case "E" => "en"
                                      case "xE" => "en"
                                      case "H" => "hi"
                                      case "xH" => "hi"
                                  }}:${p}%.3f" }.mkString(",")
                            
                            val devafiedWord = wordTranslits.get(normalizeWord(originalWord)).map(_(preferKnownTranslit)).map(_(onebest).sample()).getOrElse(normalizeWord(originalWord))

                            val guessedLang = tagDistString.lsplit(":",2).head
                            val oracleWord = guessedLang match {
                              case "en" => originalWord
                              case "hi" => devafiedWord
//                              case _ =>
//                                println(s"tagDistString=[$tagDistString]")
//                                "xxxx"
                            }
                            conllOut.writeLine(f"${i+1}\t$originalWord\t$originalWord\t$goldPos\t$goldPos\tOriginal=$originalWord|DevaWord=$devafiedWord|LangDist=$tagDistString|GoldLang=$goldTag")
                            conllOracleOut.writeLine(f"${i+1}\t$oracleWord\t$oracleWord\t$goldPos\t$goldPos\tOriginal=$originalWord|DevaWord=$devafiedWord|LangDist=$tagDistString|GoldLang=$goldTag")
                            //conllOracleOut.writeLine(f"${i+1}\t$guessedLang:$oracleWord\t$guessedLang:$oracleWord\t$goldPos\t$goldPos\tOriginal=$originalWord|DevaWord=$devafiedWord|LangDist=$tagDistString|GoldLang=$goldTag")
                                  
                            f"$originalWord|$goldTag|$tagDistString|$devafiedWord"
                        }.mkString(" "))
                        conllOut.writeLine()
                        conllOracleOut.writeLine()
                      }
                    }
                  }
                	 }
                	 }
                }
              }
            }
          }

          def doEvaluate(evalData: Vector[Vector[(String, String, String)]], outputFilePrefix: String, outputTagDistributions: Boolean = false) = {
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
                for (((word, goldLang, originalWord), predictedLang) <- (sentence zipSafe predictedTags)) {
                  confusionMatrix(goldTagIndex(goldLang))(modelTagIndex(predictedLang)) += 1
                  val cleanGoldLabel = cleanLangLabel(word, goldLang)
                  if (cleanGoldLabel != "x") {  // Just evaluate when the gold annotation is a language label.
                    if (cleanGoldLabel == cleanLangLabel(word, predictedLang)) numCorrect += 1
                      totalCount += 1
                  }
                }
                f.writeLine(sentence.zipSafe(replaceUnks(sentence)).zipSafe(predictedTags).map { case (((word, gold, originalWord), modifiedWord), predicted) => f"$originalWord|$modifiedWord|$predicted|$gold" }.mkString(" "))
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
                for (((word, goldLang, originalWord), (hmmPredictedLang, memmPredictedLang)) <- (sentence zipSafe (hmmPredictedTags zipSafe memmPredictedTags))) {
                  var str = f"$originalWord|$goldLang|$hmmPredictedLang|$memmPredictedLang"
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
