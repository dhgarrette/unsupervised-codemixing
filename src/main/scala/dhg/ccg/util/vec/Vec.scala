package dhg.ccg.util.vec

import scala.reflect.ClassTag
import scala.util.control.Breaks._
import java.util.Arrays
import scalaz._
import scalaz.Scalaz._
import scala.collection.TraversableOnce.MonadOps

trait Vec[A] extends (Int => A) {
  def apply(trueIndex: Int): A
  //def get(trueIndex: Int): Option[A]
  //final def getOrElse(trueIndex: Int, default: => A): A = get(trueIndex).getOrElse(default)
  //def containsKey(trueIndex: Int): Boolean
  def update(trueIndex: Int, e: A): Unit
  //def activeKeys: Array[Int]
  //def activeValues: Array[A]
  def activePairs: Array[(Int, A)]
  //def activeCount: Int
  def length: Int
  //final def withDefaultValue(default: A): Vec[A] = new WithDefaultValueVec(this, default)
}
