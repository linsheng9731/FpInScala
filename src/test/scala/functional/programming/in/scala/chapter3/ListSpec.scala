package functional.programming.in.scala.chapter3

import functional.programming.in.scala.chapter3
import functional.programming.in.scala.chapter3.List._
import org.scalatest._

class ListSpec extends FlatSpec with Matchers {

  val l = chapter3.List(1,2,3)
  val ld = chapter3.List(1.0, 2.0, 3.0)

  "sum" should "cal sum of myList correctly" in {
    sum(l) should be(6)
  }

  "product" should "cal product of myList correctly" in {
    product(ld) should be(6)
  }

  "tail" should "get result of myList correctly" in {
    tail(l) should be(chapter3.List(2,3))
  }

  "drop" should "drop elements of myList correctly" in {
    drop(1, l) should be(chapter3.List(2,3))
    drop(2, l) should be(chapter3.List(3))
    drop(4, l) should be(Nil)
  }

  "takeWhile" should "take elements of myList correctly" in {
    takeWhile(l){x: Int => true} should be(chapter3.List(1,2,3))
    takeWhile(l){x: Int => false} should be(chapter3.List())
    takeWhile(l){x: Int => x%2 != 0} should be(chapter3.List(1))
  }

}