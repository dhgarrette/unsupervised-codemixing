package nlp.hmm.util.vec

import scala.reflect.ClassTag
import scala.util.control.Breaks._
import java.util.Arrays
import scalaz._
import scalaz.Scalaz._
import scala.collection.TraversableOnce.MonadOps

class DirectSparseVec[@specialized(Int, Double) A: ClassTag](val activeKeysSorted: Array[Int], val data: Array[A], val activeCount: Int, val length: Int) { // extends SparseVec[A] {
  require(data.length == length)
  require(activeKeysSorted.length == activeCount)

  def apply(trueIndex: Int): A = {
    data(trueIndex)
  }

  def get(trueIndex: Int): Option[A] = {
    val idx = Arrays.binarySearch(activeKeysSorted, 0, activeCount, trueIndex)
    (idx >= 0).option(data(trueIndex))
  }

  def containsKey(trueIndex: Int): Boolean = {
    val idx = Arrays.binarySearch(activeKeysSorted, 0, activeCount, trueIndex)
    idx >= 0
  }

  def update(trueIndex: Int, e: A): Unit = {
    data(trueIndex) = e
  }

  def activeKeys = activeKeysSorted
  def activeValues = {
    val av = new Array[A](activeCount)
    var i = 0
    while (i < activeCount) {
      av(i) = data(activeKeysSorted(i))
      i += 1
    }
    av
  }

  def activePairs = {
    val r = new Array[(Int, A)](activeCount)
    var i = 0
    while (i < activeCount) {
      val ai = activeKeysSorted(i)
      r(i) = (ai, data(ai))
      i += 1
    }
    r
  }

  override def toString() = f"DirectSparseVec(${activeKeysSorted.zipWithIndex.map { case (k, i) => f"($k, ${activeValues(i)})" }.mkString(", ")})"
}
