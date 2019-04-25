package Q856

object Solution extends Solution {
  def scoreOfParentheses(s: String): Int = State(0, s).solve match {
    case None => throw new RuntimeException("Unmatched")
    case Some((_,i)) => i
  }
}

sealed trait Solution {
  case class State(i : Int, text : String) {
    def next(x : Int) : State = State(i+x, text)
    def solve : Option[(State, Int)] =
      if (i >= text.size) Some((this, 0))
      else if (i == (text.size - 1)) None
      else text(i) match {
        case '(' => this.next(1).solve match {
          case None => this.next(2).solve match {
            case None => Some((this.next(1), 1))
            case Some((s,y)) => Some((s, y+1))
          }
          case Some((p,q)) => p.next(2).solve match {
            case None =>  Some(p.next(1), 2*q)
            case Some((r : State,t)) => Some((r, 2*q + t))
          }
        }
        case _ => None
      }
  }
}
