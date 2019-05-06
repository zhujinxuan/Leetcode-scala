package Q302

object Solution extends Solution {
  def minArea(image: Array[Array[Char]], x: Int, y: Int): Int = {
    val b0 = B(x,x,y,y)
    R(b0, image, List((x,y))).process().size
  }
}

sealed trait Solution {
  case class B(l : Int, r : Int, u : Int, d : Int) {
    def take(p : (Int, Int)) : B = p match {
      case (x,y) => B(math.min(l,x), math.max(r,x), math.min(u, y), math.max(d,y))
    }
    def size : Int = (r - l + 1) * (d - u + 1)
  }

  case class R(b : B, image: Array[Array[Char]], l : List[(Int, Int)]) {
    def takeB(x: Int, y :Int) : Option[(Int, Int)] = {
      if (x < 0 || x >= image.size) None
      else {
        val d = image(x)
        if (y < 0 || y >= d.size) None
        else d(y) match {
          case '0' => None
          case _ => {
            d(y) = 0
            Some((x,y))
          }
        }
      }
    }

    def takeS(p : (Int, Int)) : List[(Int, Int)] = p match {
      case (x,y) => List((x-1, y), (x+1, y), (x, y-1), (x, y+1)) flatMap {case (x0,y0) => this.takeB(x0,y0)}
    }

    def process() : B = l match {
      case Nil => b
      case _ => {
        val l1 = l flatMap {this.takeS _}
        val b1 = l1.foldLeft[B](b) {_ take _}
        R(b1, image, l1).process()
      }
    }
  }
}
