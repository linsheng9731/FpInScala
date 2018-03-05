package functional.programming.in.scala.chapter4

import functional.programming.in.scala.chapter4
import org.scalatest.{FlatSpec, Matchers}

class EitherSpec extends FlatSpec with Matchers {

  "mean" should "return Right" in {
     val s = Seq[Double](1.0,2.0,3.0)
     chapter4.Either.mean(s) should be(chapter4.Right(2))
  }

  "mean" should "return Left" in {
    val s = Seq.empty[Double]
    chapter4.Either.mean(s) should be(chapter4.Left("mean of empty list!"))
  }

  "safe div" should "return Right" in {
    chapter4.Either.safeDiv(4, 2) should be(chapter4.Right(2))
  }

  "safe div" should "return Left" in {
    chapter4.Either.safeDiv(1, 0).isInstanceOf[chapter4.Left[ArithmeticException]] should be(true)
  }
}
