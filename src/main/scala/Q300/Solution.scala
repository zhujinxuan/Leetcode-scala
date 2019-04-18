package Q300

import scala.math.Ordering

object Solution extends Solution[Int] {
  protected[this] implicit val ord = Ordering.Int

  def lengthOfLIS(nums: Array[Int]): Int = {
    val r = nums.foldLeft[List[S]](Nil) {next(_,_)}
    r match {
      case Nil => 0
      case S(_, size) :: _ => size
    }
  }
}


sealed trait Solution[T] {
  protected[this] implicit def ord : Ordering[T]

  case class S(l : T, size : Int) {
    def take(x : T) : Option[S] =
      if (ord.gt(x, l)) Some(S(x,size+1))
      else None
  }

  def next(m : List[S], x : T) : List[S] = m match {
    case Nil => S(x,1) :: Nil
    case a :: b => {
      def nB : List[S] = next(b,x) match {
        case S(last, size) :: y if (size == a.size && ord.gt(last, a.l)) => a :: y
        case x :: y if (x.size < a.size ) => a :: x :: y
        case u => u
      }

      a take x match {
        case None => nB
        case Some(c) => c :: nB
      } } }
}
