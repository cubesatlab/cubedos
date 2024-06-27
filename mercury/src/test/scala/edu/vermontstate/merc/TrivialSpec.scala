package edu.vermontstate.merc

class TrivialSpec extends UnitSpec {

  "The universe" should "work correctly" in {
    val s = "Hello"

    assert(true != false)

    assertResult(2) {
      1 + 1
    }

    assertThrows[IndexOutOfBoundsException] {
      s.charAt(5)
    }

    val caught = intercept[IndexOutOfBoundsException] {
      s.charAt(5)
    }
    assert(caught.getMessage.indexOf("5") != -1)
  }
}
