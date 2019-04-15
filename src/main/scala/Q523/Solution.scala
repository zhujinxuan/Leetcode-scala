package Q523

import scala.collection.immutable.HashSet

object Solution {
  def checkSubarraySum(nums: Array[Int], k: Int): Boolean = {
    if (k == 0) 2 == {
      nums.foldLeft(0) {
        (count : Int, i : Int) => (count, i) match {
          case (2,_) => 2
          case (i,0) => (i + 1)
          case (_,i) => 0
        } } }
    else {
      val r = nums.foldLeft(new R(math.abs(k), HashSet(), false)) (_ take _)
      r.status
    } }
}

case class R(k : Int, c : HashSet[Int], status : Boolean) {
  def take(y : Int) : R = {
    if (status) this
    else if (y >= k) this.take(y%k)
    else if (c contains y) new R(k, HashSet(), true)
    else if ((y == 0) && (c contains k)) new R(k, HashSet(), true)
    else (new R(k, _ : HashSet[Int], false)) {
      c.foldLeft(HashSet(k - y)) {
        (m : HashSet[Int], c : Int) => (m + ((c + k - y) % k))
      } } }
}
