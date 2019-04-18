package Q153
object Solution {
  def findMin(nums: Array[Int]): Int = {
    def piv(i : Int) : D =
      if (i == 0) R
      else if (nums(i-1) > nums(i)) M
      else if (nums(i) > nums.head) R
      else L

    bsearch(piv, 0, nums.size) match {
      case None => nums(0)
      case Some(x) => nums(x)
    }
  }

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
}

