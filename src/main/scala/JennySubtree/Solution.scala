package JennySubtree
// https://www.hackerrank.com/challenges/jenny-subtrees/problem

import java.io.PrintWriter

object Solution extends Solution[Int] {
  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val nr = stdin.readLine.split(" ")

    val n = nr(0).trim.toInt

    val r = nr(1).trim.toInt

    val edges = Array.ofDim[Int](n-1, 2)

    for (edgesRowItr <- 0 until n-1) {
      edges(edgesRowItr) = stdin.readLine.split(" ").map(_.trim.toInt)
    }

    val result = jennysSubtrees(n, r, edges)

    printWriter.println(result)

    printWriter.close()
  }

  /*
   * Complete the jennysSubtrees function below.
   */
  def jennysSubtrees(n: Int, r: Int, edges: Array[Array[Int]]): Int = {
    /*
     * Write your code here.
     */
    ???
  }
}

sealed abstract class Solution[T] {
  import scala.collection.immutable.{HashMap, HashSet}

  case class Graph(links : HashMap[T, HashSet[T]]) {
    def -(i : T) : Graph = links get i match {
      case None => this
      case Some(q) => Graph apply q.foldLeft(links) {
        (m, s) => m get s match {
          case None => m
          case Some(z) => m + (s -> (z - i))
        }
      }
    }

    def subgraph(i : T, r : Int) = ???
    def getNei(i : T) : HashSet[T] = links get i match {
      case None => HashSet()
      case Some(q) => q
    }
  }

  case class P(statements : List[(HashSet[T], HashSet[T])]) {
    def intersect(lset : HashSet[T], rset: HashSet[T]) : Option[P] =
      if (lset.size != rset.size) None
      else if (lset.isEmpty) Some(this)
      else statements match {
        case Nil => Some(this)
        case (p,q) :: tail => {
          val lintersect = p & lset
          val rintersect = q & rset
          if (lintersect.size != rintersect.size) None
          else P(tail).intersect(lset diff lintersect, rset diff rintersect) match {
            case None => None
            case Some(P(tt)) => {
              val l = (p diff lintersect, q diff rintersect) :: tt
              if (lintersect.isEmpty) Some(P(l))
              else Some(P((lintersect, rintersect) :: l))
            }
          }
        }
      }

    def go[Z](f : (T, T, P) => Boolean) : Boolean = this match {
      case P(Nil) => true
      case P((lset , rset) :: tail) =>
        if (lset.isEmpty) P(tail).go(f)
        else rset.foldLeft(false) {
          (m, r) => m match {
            case true => true
            case _ => f(lset.head, r, P((lset.tail, rset - r) :: tail))
          }
        }
    }
  }

  case class GraphCompare(statements : P, gl : Graph, gr : Graph) {
    def isISO : Boolean = statements go {
      (l, r, st) => {
        val st1 = st.intersect(gl getNei l, gr getNei r)
        st1 match {
          case None => false
          case Some(stn) => GraphCompare(stn, gl - l, gr - r).isISO
        }
      }
    }
  }
}
