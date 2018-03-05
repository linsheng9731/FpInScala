package functional.programming.in.scala.chapter4

import functional.programming.in.scala.chapter4
import org.scalatest.{FlatSpec, Matchers}

class OptionSpec extends FlatSpec with Matchers {

  val s = chapter4.Some(1)
  val n = chapter4.None

  "map" should "apply function to Some correctly" in {
     s.map(x => x+1) should be(chapter4.Some(2))
  }

  "flatMap" should "apply function to Some correctly" in {
    s.flatMap(x => chapter4.Some(x+1)) should be(chapter4.Some(2))
  }

  "filter" should "return value of Some if the condition is true and return None if condition if false" in {
    s.filter(_ < 2) should be(chapter4.Some(1))
    s.filter(_ < 0) should be(chapter4.None)
  }

  "getOrElse" should "return value of Some and return default value of None" in {
    s.getOrElse(2) should be(1)
    n.getOrElse(2) should be(2)
  }

}
