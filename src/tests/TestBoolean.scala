package tests

import org.scalatest.FunSuite
import expressions.Expressions._

class TestBoolean extends FunSuite{

  test("Test Boolean 1"){
    val expression: String = "(true -> false) <> (false || false)"

    assert(evaluateBoolean(expression))
  }

  test("Test Boolean 2"){
    val expression: String = "true && false"

    assert(!evaluateBoolean(expression))
  }

  test("Test Boolean 3"){
    val expression: String = "true && true && false || true"

    assert(evaluateBoolean(expression))
  }

  test("Test Boolean 4"){
    val expression: String = "true <> (true || false xor true) || true -> false"

    assert(!evaluateBoolean(expression))
  }

}
