//package dhg.ccg.util.vec
//
//import scala.reflect.ClassTag
//import scala.util.control.Breaks._
//import java.util.Arrays
//import scalaz._
//import scalaz.Scalaz._
//import scala.collection.TraversableOnce.MonadOps
//
//class OrderedSparseVec[@specialized(Int, Double) A](val activeKeys: Array[Int], val activeValues: Array[A], vec: Vec[A], val activeCount: Int, val length: Int) extends Vec[A] {
//  def apply(trueIndex: Int): A = vec(trueIndex)
//  def get(trueIndex: Int): Option[A] = vec.get(trueIndex)
//  def containsKey(trueIndex: Int): Boolean = vec.containsKey(trueIndex)
//  def update(trueIndex: Int, e: A): Unit = { ??? }
//  def activePairs = activeKeys zip activeValues
//}
//
//object OrderedSparseVec {
//  def empty[@specialized(Int, Double) A: ClassTag](length: Int) = new OrderedSparseVec[A](new Array(0), new Array(0), SparseVec.empty[A](length), 0, length)
//
//  def apply[@specialized(Int, Double) A: ClassTag](keys: Array[Int], length: Int): OrderedSparseVec[A] = {
//    val activeCount = keys.length
//    new OrderedSparseVec(keys, new Array(activeCount), new IndirectSparseVec(keys.sorted, new Array(activeCount), activeCount, length), activeCount, length)
//  }
//
//  def apply[@specialized(Int, Double) A: ClassTag](keys: Array[Int], values: Array[A], length: Int): OrderedSparseVec[A] = {
//    val activeCount = keys.length
//    assert(values.length == activeCount, "keys and values arrays must be the same length")
//    val (activeKeysSorted, valuesBySorted) = (keys zip values).sortBy(_._1).unzip
//    new OrderedSparseVec(keys, values, new IndirectSparseVec(activeKeysSorted, valuesBySorted, activeCount, length), activeCount, length)
//  }
//}
