package org.scalatest.Q315

import org.scalatest.funspec._
import Q315.Solution.countSmaller

class QSpec extends AnyFunSpec {
  describe("Q315") {
    it("Empty") {
      assertResult(List.empty[Int]) {
        countSmaller(Array.empty)
      }
    }
  }
}
