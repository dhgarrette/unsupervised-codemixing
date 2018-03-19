package nlp.hmm.tag.learn

import dhg.util._
import math.pow
import scalaz._
import scalaz.Scalaz._
import nlp.hmm.prob._
import nlp.hmm.tagdict.TagDictionary

trait TagPriorInitializer[Tag] {
  type Word = String
  final def fromRaw(sentences: Vector[Vector[Word]], initialTagdict: TagDictionary[Tag]): LogProbabilityDistribution[Tag] = {
    val tagdict = initialTagdict.withWords(sentences.flatten.toSet)
    fromKnownSupertagSets(sentences.map(_.mapTo(tagdict.entries.getOrElse(_, Set.empty))), tagdict)
  }

  /**
   * Each token associated with its set of possible supertags, if such a set is KNOWN; if the set is unknown, it will be EMPTY.
   */
  def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Tag])]], initialTagdict: TagDictionary[Tag]): LogProbabilityDistribution[Tag]
}

class UniformTagPriorInitializer[Tag] extends TagPriorInitializer[Tag] {
  def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Tag])]], initialTagdict: TagDictionary[Tag]) = {
    new LaplaceLogProbabilityDistribution(Map(), Some(initialTagdict.allTagsSE), Some(initialTagdict.excludedTags), LogDouble(1.0), totalAddition = LogDouble(0.0))
  }
  override def toString = f"UniformTagPriorInitializer()"
}

class CheatingTagPriorInitializer[Tag](supervisedCorpus: Vector[Vector[(String, Tag)]], lambda: LogDouble) extends TagPriorInitializer[Tag] {
  def fromKnownSupertagSets(sentences: Vector[Vector[(Word, Set[Tag])]], initialTagdict: TagDictionary[Tag]) = {
    val supvTagCounts = supervisedCorpus.flatten.map(_._2).counts +
      (initialTagdict.startTag -> supervisedCorpus.size) +
      (initialTagdict.endTag -> supervisedCorpus.size)
    new LaplaceLogProbabilityDistribution(supvTagCounts.mapVals(LogDouble(_)), Some(initialTagdict.allTagsSE ++ supvTagCounts.keys), Some(initialTagdict.excludedTags), lambda)
  }
  override def toString = f"CheatingTagPriorInitializer()"
}
