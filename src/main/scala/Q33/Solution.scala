package Q33
object Solution {
  def search(nums: Array[Int], target: Int): Int = {
    def piv(i : Int) : D =
      if (i == 0) R
      else if (nums(i-1) > nums(i)) M
      else if (nums(i) > nums.head) R
      else L

    def findX(x : Int) : D =
      if (nums(x) == target) M
      else if (nums(x) < target) R
      else L

    val (start,end) = bsearch(piv, 0, nums.size) match {
      case None => (0, nums.size)
      case Some(x) =>
        if (nums.head <= target) (0, x)
        else (x, nums.size)
    }

    bsearch(findX, start, end).getOrElse(-1)
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

