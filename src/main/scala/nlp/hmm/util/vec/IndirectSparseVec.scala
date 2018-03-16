package nlp.hmm.util.vec

import scala.reflect.ClassTag
import scala.util.control.Breaks._
import java.util.Arrays
import scalaz._
import scalaz.Scalaz._
import scala.collection.TraversableOnce.MonadOps

class IndirectSparseVec[@specialized(Int, Double) A: ClassTag](val activeKeysSorted: Array[Int], val activeValues: Array[A], val activeCount: Int, val length: Int) extends SparseVec[A] {
	require(activeKeysSorted.length == activeCount)
  require(activeValues.length == length)

  def apply(trueIndex: Int): A = {
    val idx = Arrays.binarySearch(activeKeysSorted, 0, activeCount, trueIndex)
    activeValues(idx)
  }

  def get(trueIndex: Int): Option[A] = {
    val idx = Arrays.binarySearch(activeKeysSorted, 0, activeCount, trueIndex)
    (idx >= 0).option(activeValues(idx))
  }

  def containsKey(trueIndex: Int): Boolean = {
    val idx = Arrays.binarySearch(activeKeysSorted, 0, activeCount, trueIndex)
    idx >= 0
  }

  def update(trueIndex: Int, e: A): Unit = {
    val idx = Arrays.binarySearch(activeKeysSorted, 0, activeCount, trueIndex)
    activeValues(idx) = e
  }

  def activeKeys = activeKeysSorted

  def activePairs = {
    val r = new Array[(Int, A)](activeCount)
    var i = 0
    while (i < activeCount) {
      r(i) = (activeKeysSorted(i), activeValues(i))
      i += 1
    }
    r
  }

  //override def toString() = f"IndirectSparseVec($length)(${(activeKeysSorted.take(length) zip activeValues.take(length)).mkString(", ")})"
  override def toString() = f"IndirectSparseVec(${activeKeysSorted.zipWithIndex.map { case (k, i) => f"($k, ${activeValues(i)})" }.mkString(", ")})"
}

object IndirectSparseVec {
  def empty[@specialized(Int, Double) A: ClassTag](length: Int) = new IndirectSparseVec[A](new Array(0), new Array(0), 0, length)

  def apply[@specialized(Int, Double) A: ClassTag](activeKeysSorted: Array[Int], length: Int): IndirectSparseVec[A] = {
    val activeCount = activeKeysSorted.length
    IndirectSparseVec(activeKeysSorted, new Array(activeCount), activeCount, length)
  }

  def apply[@specialized(Int, Double) A: ClassTag](activeKeysSorted: Array[Int], activeValues: Array[A], length: Int): IndirectSparseVec[A] = {
    val activeCount = activeKeysSorted.length
    IndirectSparseVec(activeKeysSorted, activeValues, activeCount, length)
  }

  def apply[@specialized(Int, Double) A: ClassTag](pairs: TraversableOnce[(Int, A)], length: Int): IndirectSparseVec[A] = {
    val (activeKeysSorted, activeValues) = pairs.toArray.sortBy(_._1).unzip
    val activeCount = activeKeysSorted.length
    IndirectSparseVec(activeKeysSorted, activeValues, activeCount, length)
  }

  def apply[@specialized(Int, Double) A: ClassTag](activeKeysSorted: Array[Int], activeValues: Array[A], activeCount: Int, length: Int): IndirectSparseVec[A] = {
    assert(activeValues.length == activeCount, "keys and activeValues arrays must be the same length")
    assert(isSortedUnique(activeKeysSorted, activeCount), "keys must be unique and array must be sorted")
    if (activeCount > 0) {
      assert(0 <= activeKeysSorted(0), "no key can be less than zero")
      assert(activeKeysSorted(activeCount - 1) < length, "no key may exceed the vector length")
    }
    new IndirectSparseVec(activeKeysSorted, activeValues, activeCount, length)
  }

  def isSortedUnique(a: Array[Int], len: Int): Boolean = {
    if (len == 0) return true
    var i = 1
    breakable {
      while (i < len) {
        if (a(i - 1) >= a(i)) break
        i += 1
      }
    }
    return i == len
  }
}
