package Q55

object Solution extends Solution {
  def canJump(nums: Array[Int]): Boolean =
    if (nums.isEmpty) true
    else {
      val r = nums.foldLeft[R](Process(0,0,nums.size-1)) {_ take _}
      r match {
        case Succ(_) => true
        case _ => false
      }
    }
}

sealed trait Solution {
  abstract class R {
    def take(step : Int) : R = this match {
      case Fail(_) => this
      case Succ(_) => this
      case Process(m, i, l) =>
        if (i < m) Fail(m)
        else math.max(m, i + step) match {
          case y if (y >= l) => Succ(y)
          case y => Process(y,i+1,l)
        }
    }
  }
  case class Fail(i : Int) extends R
  case class Succ(i : Int) extends R
  case class Process(max : Int, index : Int, last : Int) extends R
}
