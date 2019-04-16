package PCache

object Solution {
  import scala.collection.immutable.HashMap
  // x : List Time ItemId
  def solve(x : List[(Int, Int)]) : List[Int] = {
    val result = x.foldLeft[Result](Result(HashMap(), Time(0))) { (m, n) => m.bang(Time(n._1), n._2) }
    result.getCache().map(_._1)
  }

  case class Time(t : Int) {
    def diff(y : Time) : Int = t - y.t
  }

  case class Priority(t : Time, p : Int, inCache : Boolean) {
    def elapse(now : Time) : Priority = {
      val newP = Math.max(0, p - now.diff(t))
      if (!inCache) Priority(now, newP, false)
      else Priority(now, newP, (newP > 3))
    }

    def bang(now : Time) : Priority = {
      val (newP,outC) = now.diff(t) match {
        case 0 => (2+p, false)
        case x => (Math.max(0, p - x) + 2, (p - x + 1) <= 3)
      }

      if (newP > 5) Priority(now,newP, true)
      else if (!inCache) Priority(now,newP,false)
      else Priority(now, newP, !outC)
    }
  }

  case class Result(logs : HashMap[Int, Priority], now : Time) {
    def bang(time : Time, id : Int) : Result = (Result(_ : HashMap[Int, Priority], time)) {
      logs get id match {
        case None => logs + (id -> Priority(time, 2, false))
        case Some(p) => logs + (id -> p.bang(time))
      }
    }

    def getCache() : List[(Int, Int)] = {
      def insert(i : (Int, Int), l : List[(Int,Int)]) : List[(Int,Int)] = l match {
        case Nil => i :: Nil
        case (_,p) :: _ if p >= i._2 => i :: l
        case x :: y => x :: insert(i,y)
      }

      logs.foldLeft[List[(Int,Int)]](Nil) {
        (m, p) => p._2.elapse(now) match {
          case Priority(_,_,false) => m
          case Priority(_,w,true) => insert((p._1, w), m)
        } } }
  }
}
