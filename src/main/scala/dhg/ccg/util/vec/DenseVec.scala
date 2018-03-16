//package dhg.ccg.util.vec
//
//import scala.reflect.ClassTag
//import scala.util.control.Breaks._
//import java.util.Arrays
//import scalaz._
//import scalaz.Scalaz._
//import scala.collection.TraversableOnce.MonadOps
//
//class DenseVec[@specialized(Int, Double) A](val values: Array[A], val length: Int) extends Vec[A] {
//  def apply(trueIndex: Int): A = {
//    activeValues(trueIndex)
//  }
//
//  def get(trueIndex: Int): Option[A] = {
//    (0 <= trueIndex && trueIndex < length).option(activeValues(trueIndex))
//  }
//
//  def containsKey(trueIndex: Int): Boolean = {
//    (0 <= trueIndex && trueIndex < length)
//  }
//
//  def update(trueIndex: Int, e: A): Unit = {
//    activeValues(trueIndex) = e
//  }
//
//  def keys: Array[Int] = (0 to length).toArray
//  def activeKeys = keys
//  val activeValues = values
//  def activeCount: Int = length
//
//  def activePairs = {
//    val r = new Array[(Int, A)](length)
//    var i = 0
//    while (i < length) {
//      r(i) = (i, values(i))
//      i += 1
//    }
//    r
//  }
//}
//
//object DenseVec {
//  def empty[@specialized(Int, Double) A: ClassTag](length: Int) = new DenseVec[A](new Array(0), length)
//
//  def apply[@specialized(Int, Double) A: ClassTag](values: Array[A]): DenseVec[A] = {
//    new DenseVec(values, values.length)
//  }
//
//  def apply[@specialized(Int, Double) A: ClassTag](values: Array[A], length: Int): DenseVec[A] = {
//    assert(values.length == length, "values vector is the wrong length")
//    new DenseVec(values, length)
//  }
//
//  def apply[@specialized(Int, Double) A: ClassTag](length: Int): DenseVec[A] = {
//    new DenseVec(new Array[A](length), length)
//  }
//
//  def apply[@specialized(Int, Double) A: ClassTag](pairs: TraversableOnce[(Int, A)], length: Int): DenseVec[A] = {
//    val values = new Array[A](length)
//    for ((k, v) <- pairs) {
//      values(k) = v
//    }
//    new DenseVec(values, length)
//  }
//}
