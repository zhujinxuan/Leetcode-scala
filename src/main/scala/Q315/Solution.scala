// https://leetcode.com/problems/count-of-smaller-numbers-after-self/
package Q315
object Solution extends Solution[Int] {

  def countSmaller(nums: Array[Int]): List[Int] = {
    val(res,_) = nums.foldRight((List[Int](), Empty : BT)) {
      case (a, (r, tree)) => {
        val (head, t) = tree insert a
        (head::r, t)
      }
    }

    res
  }

}

sealed trait Solution[T] {

  import math.Ordered.orderingToOrdered

  sealed trait BT {
    val size : Int

    def insert(t : T)(implicit ord : Ordering[T]) : (Int, BT) = this match {
      case Empty => (0, Tree(t, Empty, Empty, 1))
      case Tree(v, l, r, c) => {
        if (t == v) {
          (l.size, Tree(v, l, r, c+1))
        } else if (t < v) {
          // Insert left
          val (smaller, newl) = l insert t
          (smaller, Tree(v, newl, r, c))
        } else {
          // Insert right
          val (smaller, newr) = r insert t
          (l.size + c + smaller, Tree(v, l, newr, c))
        }
      }
    }
  }

  case object Empty extends BT {
    val size = 0
  }
  case class Tree(v: T, l: BT, r: BT, count : Int) extends BT {
    val size = l.size + r.size + count
  }

}
