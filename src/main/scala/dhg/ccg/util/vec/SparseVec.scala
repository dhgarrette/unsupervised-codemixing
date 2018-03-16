package dhg.ccg.util.vec

import scala.reflect.ClassTag
import scala.util.control.Breaks._
import java.util.Arrays
import scalaz._
import scalaz.Scalaz._
import scala.collection.TraversableOnce.MonadOps

trait SparseVec[A] extends Vec[A] {
  //def activeKeysSorted: Array[Int];
}

object SparseVec {
  //def empty[@specialized(Int, Double) A: ClassTag](length: Int): Vec[A] = DenseVec.empty[A](length)

  def makeVec[@specialized(Int, Double) A: ClassTag](activeKeysSorted: Array[Int], activeValues: Array[A], activeCount: Int, length: Int): Vec[A] = {
    if (activeCount * 100 / length < 80) {
      new IndirectSparseVec(activeKeysSorted, activeValues, activeCount, length)
    }
    else {
      //      val values = new Array[A](length)
      //      var ai = 0
      //      while (ai < activeCount) {
      //        values(activeKeysSorted(ai)) = activeValues(ai)
      //        ai += 1
      //      }
      //      new DenseVec(values, length)
      ???
    }
  }

}
