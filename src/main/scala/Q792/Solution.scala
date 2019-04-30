package Q792
import scala.collection.immutable.HashMap
object Solution extends Solution {
  def numMatchingSubseq(S: String, words: Array[String]): Int = {

    val r0 = words.foldLeft(R(0, HashMap())) {(r, w) => r.add(w.size, w)}
    val r = S.foldLeft(r0) {_ take _}

    r.out
  }
}

sealed trait Solution {
    case class R(out : Int, x : HashMap[Char, List[(Int, String)]]) {
    def add(i : Int, s : String) : R =
      if (i == 0) R(out + 1, x)
      else {
        val char = s(s.size - i)
        val x1 = x get char match {
          case None => x + (char -> List((i, s)))
          case Some(p) => x + (char -> ((i,s) :: p))
        }
        R(out, x1)
      }

    def take(c : Char) : R = x get c match {
      case None => this
      case Some(p) => p.foldLeft[R](R(out, x - c)) {
        (m, y) => m.add(y._1 -1, y._2)
      }
    }
  }
}
