package nlp.hmm.tag

import dhg.util._
import com.typesafe.scalalogging.{ StrictLogging => Logging }
import math.{ log, exp }
import scalaz._
import Scalaz._
import nlp.hmm.prob._
import nlp.hmm.tag.learn._
import nlp.hmm.tagdict.TagDictionary
import nlp.hmm.tagdict.SimpleTagDictionary
import nlp.hmm.tagdict.SimpleTagDictionaryFactory

class HmmTagger[Tag](
  val transitions: ConditionalLogProbabilityDistribution[Tag, Tag],
  val emissions: ConditionalLogProbabilityDistribution[Tag, String],
  val tagdict: TagDictionary[Tag])
  extends WeightedTagger[Tag]
  with Tagger[Tag]
  with Logging {

  /**
   * Compute the probability of the tagged sentence.  The result
   * should be represented as a logarithm.
   */
  override def sentenceProbWithWeights(sentence: Vector[(Word, Tag)], us: Vector[Map[Tag, LogDouble]]): LogDouble = {
    (((tagdict.startWord -> tagdict.startTag) +: sentence :+ (tagdict.endWord -> tagdict.endTag)) zipSafe (Map.empty[Tag, LogDouble] +: us :+ Map.empty[Tag, LogDouble]))
      .sliding2.foldLeft(LogDouble.one) {
        case (logProd, (((_, prevTag), _), ((currWord, currTag), u))) =>
          logProd * transitions(currTag, prevTag) * emissions(currWord, currTag) / u.getOrElse(currTag, LogDouble.zero)
      }
  }

  /**
   * Accepts a sentence of word tokens and returns a sequence of
   * tags corresponding to each of those words.
   */
  override def tagAndProbWithWeights(sentence: Vector[Word], us: Vector[Map[Tag, LogDouble]]): (Vector[Tag], LogDouble) = {
    tagAndProbWithWeightsFromTagSet(sentence.mapTo(tagdict), us)
  }

  /**
   * Accepts a sentence of word tokens and returns a sequence of
   * tags corresponding to each of those words.
   */
  override def tagAndProbWithWeightsFromTagSet(sentence: Vector[(Word, Set[Tag])], us: Vector[Map[Tag, LogDouble]]): (Vector[Tag], LogDouble) = {
    val forwards: Vector[Map[Tag, (LogDouble, Tag)]] =
      ((sentence :+ ((tagdict.endWord, Set(tagdict.endTag)))) zipSafe (us :+ Map.empty))
        .scanLeft(Map(tagdict.startTag -> (LogDouble.one, tagdict.startTag))) {
          case (prevV, ((currWord, potentialTags), u)) =>
            //logger.debug(f"currWord = $currWord")
            //logger.debug(f"  prev viterbi = $prevV")
            val v: Map[Tag, (LogDouble, Tag)] =
              potentialTags.mapTo { k =>
                //logger.debug(f"  k=$k")
                val scores =
                  prevV.map {
                    case (kprime, (kprimeScore, _)) =>
                      //assert(transitions(k, kprime).isNegInfinity, f"transitions($k, $kprime) = ${transitions(k, kprime)}")
                      //println(f"transitions($k, $kprime) = ${transitions(k, kprime)}")
                      val score = kprimeScore * transitions(k, kprime)
                      //if (currWord == "the" && kprime == "V" && k == "D") { // TODO: REMOVE
                      //logger.debug(f"    $currWord  $kprime%-3s -> $k%-3s  $score")
                      //logger.debug(f"      v($kprime) = ${kprimeScore}")
                      //logger.debug(f"      tr($kprime -> $k) = ${log(transitions(k, kprime))}")
                      //}
                      kprime -> score
                  }
                val (bestKprime, bestKprimeScore) = scores.maxBy(_._2)
                //assert(!bestKprimeScore.isNegInfinity, f"oh shit: bestKprimeScore=$bestKprimeScore, currWord=$currWord, k=$k, prevTags=${prevV.map(_._1)}")
                //if ((currWord == "the" && k == "D") || currWord == "held") { // TODO: REMOVE
                //logger.debug(f"    scores: $scores -> $k")
                //logger.debug(f"    best: $bestKprime%-3s -> $k%-3s  bestKprimeScore=$bestKprimeScore")
                //logger.debug(f"    em($currWord | $k) = ${emissions(currWord, k)}")
                //}
                //assert(emissions(currWord, k).isNegInfinity, f"emissions($currWord, $k) = ${emissions(currWord, k)}")
                //println(f"emissions($currWord, $k) = ${emissions(currWord, k)}")
                (emissions(currWord, k) * bestKprimeScore / u.getOrElse(k, LogDouble.one), bestKprime)
              }.toMap
            //logger.debug(f"  v = $v")
            v
        }
    val tags =
      forwards.scanRight(tagdict.endTag) {
        (v, kNext) => v(kNext)._2
      }.drop(2).dropRight(1) // drop start/end tags
    val p = forwards.last(tagdict.endTag)._1
    (tags, p)
  }

  /**
   * Accepts a sentence of word tokens and returns a sequence of
   * tag probabilities corresponding to each of those words.
   */
  def tagToProbDistsFromTagSet(sentence: Vector[(Word, Set[Tag])]): Vector[Map[Tag, LogDouble]] = {
    val forwards =
      (sentence :+ ((tagdict.endWord, Set(tagdict.endTag))))
        .scanLeft(Map(tagdict.startTag -> LogDouble.one)) {
          case (prevV, (currWord, potentialKs)) =>
            val v =
              potentialKs.mapTo { k =>
                val scores =
                  prevV.map {
                    case (kprime, kprimeScore) =>
                      // tr(k' -> k) * forward_{i-1}(k')
                      transitions(k, kprime)  * kprimeScore
                  }
                emissions(currWord, k) * scores.sum
              }.toMap
            v
        }
    val Coll(_ -> pForward) = forwards.last
//    println(f"FORWARDS: $pForward")
//    for ((word, forward) <- (tagdict.startWord +: sentence.map(_._1) :+ tagdict.endWord).zipSafe(forwards)) {
//        println(f"$word%20s ${forward.toVector.map { case (tag, p) => f"$tag: $p" }.mkString(" ")}")
//    }
    val backwards =
      ((sentence.map(_._1) :+ tagdict.endWord) zipSafe (Set(tagdict.startTag) +: sentence.map(_._2)))
        .scanRight(Map(tagdict.endTag -> LogDouble.one)) {
          case ((nextWord: Word, potentialKs: Set[Tag]), nextV) =>
            val v =
              potentialKs.mapTo { k =>
                val scores =
                  nextV.map {
                    case (kprime, kprimeScore) =>
                      // tr(k -> k') * em(k' -> w_{i+1}) * backward_{i+1}(k')
                      transitions(kprime, k) * emissions(nextWord, kprime) * kprimeScore
                }
                scores.sum
              }.toMap
            v
        }
    val Coll(_ -> pBackward) = backwards.head
//    println(f"BACKWARD: $pBackward")
//    for ((word, backward) <- (tagdict.startWord +: sentence.map(_._1) :+ tagdict.endWord).zipSafe(backwards)) {
//        println(f"$word%20s ${backward.toVector.map { case (tag, p) => f"$tag: $p" }.mkString(" ")}")
//    }
    assert(math.abs(pForward.toDouble - pBackward.toDouble) < 0.000000001)
    (sentence zipSafe (forwards zipSafe backwards).drop(1).dropRight(1)).map {
      case ((_, potentialKs), (currForward, currBackward)) =>
        potentialKs.mapTo { k =>
          currForward(k) * currBackward(k) / pForward
        }.toMap
    }
  }
}

trait SupervisedHmmTaggerTrainer[Tag] extends SupervisedTaggerTrainer[Tag] {
  def make(
    transitions: ConditionalLogProbabilityDistribution[Tag, Tag],
    emissions: ConditionalLogProbabilityDistribution[Tag, Word],
    tagdict: TagDictionary[Tag]) = new HmmTagger(transitions, emissions, tagdict)
}

class UnsmoothedHmmTaggerTrainer[Tag]() extends SupervisedHmmTaggerTrainer[Tag]() {
  private[this] val dister = new SmoothedHmmTaggerTrainer[Tag](new UnsmoothedTransitionDistributioner(), new UnsmoothedEmissionDistributioner())
  override def train(taggedSentences: Vector[Vector[(Word, Tag)]], initialTagdict: TagDictionary[Tag]): HmmTagger[Tag] = {
    dister.train(taggedSentences, initialTagdict)
  }
}

class AddLambdaSmoothedHmmTaggerTrainer[Tag](lambda: Double) extends SupervisedHmmTaggerTrainer[Tag] {
  private[this] val dister = new SmoothedHmmTaggerTrainer[Tag](new AddLambdaTransitionDistributioner(lambda), new AddLambdaEmissionDistributioner(lambda))
  override def train(taggedSentences: Vector[Vector[(Word, Tag)]], initialTagdict: TagDictionary[Tag]): HmmTagger[Tag] = {
    dister.train(taggedSentences, initialTagdict)
  }
}

class SmoothedHmmTaggerTrainer[Tag](
  transitionDister: TransitionDistributioner[Tag], emissionDister: EmissionDistributioner[Tag])
  extends SupervisedHmmTaggerTrainer[Tag]() {

  override def train(taggedSentences: Vector[Vector[(Word, Tag)]], initialTagdict: TagDictionary[Tag]): HmmTagger[Tag] = {
    val tagdict = initialTagdict.withWords(taggedSentences.flatten.map(_._1).toSet).withTags(taggedSentences.flatten.map(_._2).toSet)

    val transitions = transitionDister(taggedSentences, tagdict)
    val emissions = emissionDister(taggedSentences, tagdict)

    //    println("\nTRANSITIONS\n")
    //    for (t1 <- (allTags + startTag + endTag)) {
    //      for (t2 <- ((allTags + startTag + endTag)).toVector.sortBy(t2 => -transitions(t2, t1))) {
    //        println(f"$t1%-5s -> $t2%-10s  ${transitions(t2, t1)}%.2f  ${log(transitions(t2, t1))}%.2f")
    //      }
    //      println
    //    }

    //    println("\nEMISSIONS\n")
    //    for (t <- (allTags + startTag + endTag)) {
    //      for (w <- ((allWords + startWord + endWord) + "<DEFAULT>").toVector.sortBy(w => -emissions(w, t))) {
    //        println(f"$t%-5s -> $w%-10s  ${emissions(w, t)}%.2f  ${log(emissions(w, t))}%.2f")
    //      }
    //      println
    //    }

    new HmmTagger(transitions, emissions, tagdict)
  }
}

object Hmm extends Logging {

  /** Returns a vector of tagged sentences */
  def taggedSentencesFile(filename: String) = {
    File(filename).readLines.zipWithIndex.map {
      case (line, lineNum) =>
        line.split("\\s+")
          .map(_.split("\\|"))
          .map {
            case Array(w, t) => (w, t)
            case x => sys.error(f"failed on line $lineNum")
          }.toVector
    }.toVector
  }

  def main(args: Array[String]): Unit = {
    val (arguments, options) = parseArgs(args)

    val trainData = taggedSentencesFile(options("train"))

    val td = {
      val tdcutoffProvided = options.get("tdcutoff").isDefined
      val tdCutoff =
        options.get("tagdict").fold(tdcutoffProvided)(_.toBoolean).option {
          options.get("tdcutoff").fold(0.0)(_.toDouble)
        }
      new SimpleTagDictionaryFactory(tdCutoff)(trainData, "<S>", "<S>", "<E>", "<E>")
    }

    val trainer: SupervisedTaggerTrainer[String] = {
      val lambda = options.get("lambda").fold(1.0)(_.toDouble)
      if (options.contains("lambda") && !options.contains("tsmooth") && !options.contains("esmooth")) {
        new AddLambdaSmoothedHmmTaggerTrainer[String](lambda)
      }
      else if (options.contains("tsmooth") || options.options.contains("esmooth")) {
        val tsmooth: TransitionDistributioner[String] =
          options.s("tsmooth", "none") match {
            case "addlambda" => new AddLambdaTransitionDistributioner(lambda)
            case "onecount" => new OneCountTransitionDistributioner(lambda, lambda)
            case "none" | "unsmoothed" | "un" => new UnsmoothedTransitionDistributioner()
          }
        val esmooth: EmissionDistributioner[String] =
          options.s("esmooth", "none") match {
            case "addlambda" => new AddLambdaEmissionDistributioner(lambda)
            case "onecount" => new OneCountEmissionDistributioner(lambda, lambda)
            case "none" | "unsmoothed" | "un" => new UnsmoothedEmissionDistributioner()
          }
        new SmoothedHmmTaggerTrainer(tsmooth, esmooth)
      }
      else {
        new UnsmoothedHmmTaggerTrainer()
      }
    }

    val model = time("training", trainer.train(trainData, td))
    TaggerEvaluator(model, taggedSentencesFile(options("test")))
  }

}
