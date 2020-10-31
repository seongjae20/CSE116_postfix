package tests

import org.scalatest.FunSuite
import expressions.Expressions._

class TestArithmetic extends FunSuite{

  val EPSILON: Double = 0.000001

  def equalDoubles(d1: Double, d2: Double): Boolean = {
    (d1 - d2).abs < EPSILON
  }

  test("Test Arithmetic 1"){
    val expression = "10 - (8/12.0*6)/2-1"

    assert(equalDoubles(evaluateArithmetic(expression), 7.0))
  }

  test("Test Arithmetic 2"){
    val expression = "2.5+2.5"

    assert(equalDoubles(evaluateArithmetic(expression), 5.0))
  }

  test("Test Arithmetic 3"){
    val expression = "2 ^ 3 ^ 3  * 2"

    assert(equalDoubles(evaluateArithmetic(expression), 1024.0))
  }

}
