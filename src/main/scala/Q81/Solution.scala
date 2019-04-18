package Q81
/**
  * Test Case
  * [5,1,3]
  * 3
  * [1,2,1]
  * 2
  * [1]
  * 1
  */

object Solution extends Solution {
  def search(n1: Array[Int], target: Int): Boolean = {
    val nums = OffsetArray(n1)

    def piv(i : Int) : D =
      if (i == 0) R
      else if (nums(i-1) > nums(i)) M
      else if (nums(i) > nums.head) R
      else L

    def findX(x : Int) : D =
      if (nums(x) == target) M
      else if (nums(x) < target) R
      else L

    if (nums.isEmpty)
      if (n1.isEmpty) false
      else n1.head == target
    else if (nums.head == target) true
    else {
      val (start,end) = bsearch(piv, 0, nums.size) match {
        case None => (0, nums.size)
        case Some(x) =>
          if (nums.head < target) (0, x)
          else (x, nums.size)
      }
      bsearch(findX, start, end).isEmpty == false
    }
  }
}

sealed abstract class Solution {
  sealed abstract class D {}
  case object L extends D {}
  case object R extends D {}
  case object M extends D {}

  /**
    *  end is exclusive
    */
  def bsearch(d : Int => D, start : Int, end : Int) : Option[Int] =
    if (end <= start) None
    else {
      val mid : Int = (start + end)/2
      d(mid) match {
        case M => Some(mid)
        case L => bsearch(d, start, mid)
        case R => bsearch(d, mid + 1, end)
      } }
  case class OffsetArray[T](nums : Array[T]) {
    val offset : Int =
      if (nums.isEmpty) 0
      else nums.indexWhere(_ != nums.head) match {
        case -1 => nums.size
        case i => (i - 1)
      }
    val size : Int = nums.size - offset
    val isEmpty : Boolean = nums.size == offset
    def apply(x : Int) : T = nums(x + offset)
    def head : T = nums(offset)
  }
}
