package functional.programming.in.scala.chapter5

import org.scalatest.{FlatSpec, Matchers}

class StreamSpec extends FlatSpec with Matchers {

  val s = Stream(1,2,3)

  "headOption" should "return Some(1)" in {
    s.headOption should be(Some(1))
  }

  "toList" should "return list(1,2,3)" in {
    s.toList should be(List(1,2,3))
    s.toListRecur should be(List(1,2,3))
  }

  "drop" should "return list(2,3)" in {
    s.drop(1).toList should be(List(2,3))
  }

  "take" should "return list(1,2)" in {
    s.take(2).toList should be(List(1,2))
  }

  "takeWhile" should "return list(1)" in {
    s.takeWhile(x => x % 2 != 0).toList should be(List(1))
  }

  "constant" should "return Stream[A]" in {
    Stream.constant(3).take(3).toList should be(List(3,3,3))
  }

  "from" should "return Stream[A]" in {
    Stream.from(3).take(3).toList should be(List(3,4,5))
  }

  "fibs" should "return list(1,1,2,3,5)" in {
    Stream.fibs(1,1).take(5).toList should be(List(1,1,2,3,5))
  }

}
