package nlp.hmm.tag

import org.junit.Test
import dhg.util._
import org.junit.Assert._
import nlp.hmm.tag.learn.SoftEmHmmTaggerTrainer
import nlp.hmm.tag.learn.UnsmoothedEmissionDistributioner
import nlp.hmm.tagdict.SimpleTagDictionary
import nlp.hmm.prob.SimpleConditionalLogProbabilityDistribution
import nlp.hmm.prob.SimpleLogProbabilityDistribution
import nlp.hmm.tag.learn.UnsmoothedTransitionDistributioner
import nlp.hmm.prob.LogProbabilityDistribution

class HmmTaggerTests {

  @Test
  def test_SoftEmHmm_toy_train_2iterations {
    val scale = 10

    val sentences = Vector(
      "a cat chases the dog",
      "the dog walks",
      "the man walks the dog",
      "the man runs").map(_.lsplit(" "))
    val tagdict = SimpleTagDictionary(Map(
      "the" -> Set('D),
      "a" -> Set('D),
      "every" -> Set('D),
      "some" -> Set('D),
      "man" -> Set('N, 'V),
      "cat" -> Set('N),
      "bird" -> Set('N),
      "fox" -> Set('N),
      "walks" -> Set('V),
      "flies" -> Set('N, 'V)),
      "<S>", 'A, "<E>", 'Z)
      .withWords(sentences.flatten.toSet)

    val trInit =
      new SimpleConditionalLogProbabilityDistribution[Symbol, Symbol](
        Map(
          tagdict.startTag -> new SimpleLogProbabilityDistribution(Map('D -> 0.7, 'N -> 0.2, 'V -> 0.1).mapVals(LogDouble(_))),
          'D -> new SimpleLogProbabilityDistribution(Map('D -> 0.1, 'N -> 0.7, 'V -> 0.1, tagdict.endTag -> 0.1).mapVals(LogDouble(_))),
          'N -> new SimpleLogProbabilityDistribution(Map('D -> 0.1, 'N -> 0.3, 'V -> 0.4, tagdict.endTag -> 0.2).mapVals(LogDouble(_))),
          'V -> new SimpleLogProbabilityDistribution(Map('D -> 0.3, 'N -> 0.2, 'V -> 0.1, tagdict.endTag -> 0.4).mapVals(LogDouble(_))),
          tagdict.endTag -> LogProbabilityDistribution.empty))
    val emInit =
      new SimpleConditionalLogProbabilityDistribution[Symbol, String](
        Map(
          'D -> new SimpleLogProbabilityDistribution(Map("the" -> 0.4, "a" -> 0.3, "dog" -> 0.1, "chases" -> 0.1, "runs" -> 0.1).mapVals(LogDouble(_))),
          'N -> new SimpleLogProbabilityDistribution(Map("cat" -> 0.2, "man" -> 0.3, "dog" -> 0.2, "chases" -> 0.2, "runs" -> 0.1).mapVals(LogDouble(_))),
          'V -> new SimpleLogProbabilityDistribution(Map("man" -> 0.2, "dog" -> 0.1, "walks" -> 0.2, "chases" -> 0.3, "runs" -> 0.2).mapVals(LogDouble(_))),
          tagdict.startTag -> new SimpleLogProbabilityDistribution(Map(tagdict.startWord -> LogDouble.one)),
          tagdict.endTag -> new SimpleLogProbabilityDistribution(Map(tagdict.endWord -> LogDouble.one))))

    val hmm = new HmmTagger(trInit, emInit, tagdict)
    val tagProbs = hmm.tagToProbDistsFromTagSet(sentences(0).mapTo(tagdict))

    //    for (t1 <- tagdict.allTags.toVector.sortBy(_.toString))
    //      println(f"$t1 -> ${tagdict.allTags.toVector.sortBy(_.toString).mapTo(t2 => f"${tr(t2, t1)}%.2f").mkString(" ")}")
    //    for (t <- tagdict.allTags.toVector.sortBy(_.toString))
    //      println(f"$t -> ${tagdict.allWords.toVector.sortBy(_.toString).mapTo(w => f"${em(w, t)}%.2f").mkString(" ")}")

    assertEquals(1, tagProbs(0).size)
    assertEquals(1.0, tagProbs(0)('D).toDouble, 1e-9)

    assertEquals(1, tagProbs(1).size)
    assertEquals(1.0, tagProbs(1)('N).toDouble, 1e-9)
    
    assertEquals(3, tagProbs(2).size)
    assertEquals(0.0232558140, tagProbs(2)('D).toDouble, 1e-9)
    assertEquals(0.1395348837, tagProbs(2)('N).toDouble, 1e-9)
    assertEquals(0.8372093023, tagProbs(2)('V).toDouble, 1e-9)

    assertEquals(1, tagProbs(3).size)
    assertEquals(1.0, tagProbs(3)('D).toDouble, 1e-9)

    assertEquals(3, tagProbs(4).size)
    assertEquals(0.0303030303, tagProbs(4)('D).toDouble, 1e-9)
    assertEquals(0.8484848485, tagProbs(4)('N).toDouble, 1e-9)
    assertEquals(0.1212121212, tagProbs(4)('V).toDouble, 1e-9)
  }

}
