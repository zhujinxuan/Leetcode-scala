package Q327

import scala.math.Numeric

object Solution extends Solution[BigInt] {
  def countRangeSum(nums: Array[Int], lower: Int, upper: Int): Int =
    if (nums.isEmpty) 0
    else {
      val ra = new Ra(0, BigInt(lower), BigInt(upper + 1))
      val st = initStore(nums.map(BigInt(_)))
      val res = nums.foldLeft[Res](Res(0, st, ra)) {_ drop _}
      res.drop(0).count
    }
}

sealed trait Solution[T](implicit num: Numeric[T]) {
  import scala.collection.immutable.TreeMap
  import scala.language.implicitConversions
  import num._

  case class Ra(count : Int, min : T, max : T) {
    def drop(x : T) : Ra = Ra(count + 1, min + x, max + x)
  }

  type TM = TreeMap[T, List[Int]]
  case class Store(s : TM) {
    def count(ra : Ra) : Int = s.range(ra.min, ra.max).foldLeft(0) {
      (m, c) => c._2.foldLeft(m) { (m1, x) => if (x > ra.count) m1 +1 else m1 }
    }
  }

  case class Res(count : Int, s : Store, ra : Ra) {
    def drop(x : T) : Res = Res(count + s.count(ra), s, ra.drop(x))
  }

  def initStore(nums : Array[T]) : Store = {
    val sum0 : T = nums.foldLeft(zero) {_ + _}
    val (tm : TM,_,_) = nums.foldRight[(TM, T, Int)]( (TreeMap(), sum0, nums.size)) {
      (a, m) => {
        val (tm, sum, length) = m
        val delta = tm get sum match {
          case None => length :: Nil
          case Some(q) => length :: q
        }
        (tm + (sum -> delta), sum - a, length - 1)
      }
    }
    Store(tm)
  }
}
