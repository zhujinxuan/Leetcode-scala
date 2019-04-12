package Q446

import scala.collection.immutable.HashMap

object Solution {
  def numberOfArithmeticSlices(a: Array[Int]): Int = {
    val emptyT = new Table(HashMap.empty)
    val empty = new Result(List(), emptyT, emptyT)
    val result = a.foldLeft(empty) {case (m,x) => m.take(x)}
    result.count
  }
}

case class Count(i : Int) {
  def +(y : Count) : Count = y match {
    case Count(0) => this
    case Count(n) => new Count(i+n)
  }
}

case class Delta(d : HashMap[BigInt, Count]) {
  def take(delta : BigInt, count : Count) : Delta = (new Delta(_)) {
    d get delta match {
      case None => d + (delta -> count)
      case Some(n) => d + (delta -> (count + n))
    } }
}

case class Table(table : HashMap[BigInt, Delta]) {
  def take(x : BigInt) : Table = table get x match {
    case None => this
    case Some(Delta(d)) => d.foldLeft(this) {
      case (m : Table, (delta, count)) => m.update(x + delta, delta,count)
    } }

  def update(expect : BigInt, delta : BigInt, c : Count) : Table = ((r:Delta) => new Table(table + (expect -> r))) {
    table get expect match {
      case None => new Delta(HashMap((delta, c)))
      case Some(Delta(d)) => ((r:Count) => new Delta(d + (delta -> r))) {
        d get delta match {
          case None => c
          case Some(c1) => c + c1
        } } } }

  def recieve(p : Table, x : BigInt) : Table = p.table get x match {
      case None => this
      case Some(Delta(d)) => d.foldLeft(this) {
        (m : Table, q : (BigInt, Count)) => m.update(x + q._1, q._1, q._2)
      } }

  def recieve0(ints : List[BigInt], x : BigInt) : Table = ints.foldLeft(this) {
    (m : Table, i : BigInt) => {
      val d = x - i
      val e = x + d
      m.update(e, d, new Count(1))
    } }

  lazy val count : Int = table.foldLeft(0) {
    case (m : Int, (_, Delta(d))) => d.foldLeft(m) {
      case (n : Int, (_, Count(i))) => (n + i)
    } }
}

case class Result(ints : List[BigInt], pairs : Table, table : Table) {
  def take(x : BigInt) : Result = {
    val newP = pairs.recieve0(ints, x)
    val newT = table.take(x).recieve(pairs,x)
    new Result(x :: ints, newP, newT)
  }
  def count : Int = table.count
}
