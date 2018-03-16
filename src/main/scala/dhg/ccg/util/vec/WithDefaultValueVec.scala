package dhg.ccg.util.vec

import scala.reflect.ClassTag
import scala.util.control.Breaks._
import java.util.Arrays
import scalaz._
import scalaz.Scalaz._
import scala.collection.TraversableOnce.MonadOps

class WithDefaultValueVec[@specialized(Int, Double) A](delegate: Vec[A], default: A) extends Vec[A] {
  def apply(trueIndex: Int): A = ??? // delegate.getOrElse(trueIndex, default)
  //def get(trueIndex: Int): Option[A] = Some(apply(trueIndex))
  //def containsKey(trueIndex: Int): Boolean = delegate.containsKey(trueIndex)
  def update(trueIndex: Int, e: A): Unit = { delegate(trueIndex) = e }
  //def activeKeys: Array[Int] = delegate.activeKeys
  //def activeValues: Array[A] = delegate.activeValues
  def activePairs: Array[(Int, A)] = delegate.activePairs
  //def activeCount: Int = delegate.activeCount
  def length: Int = delegate.length
}
