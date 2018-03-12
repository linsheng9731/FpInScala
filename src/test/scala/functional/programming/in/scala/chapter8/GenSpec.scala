package functional.programming.in.scala.chapter8

import org.scalatest.{FlatSpec, Matchers}
import functional.programming.in.scala.chapter8.Prop._

/**
  * GenSpec
  *
  * @author damon lin
  *         2018/3/12
  */
class GenSpec extends FlatSpec with Matchers {

  "Gen" should "generate value in right way" in  {
    val smallInt = Gen.choose(-10, 10)
    val p1 = forAll(smallInt){ ns =>
      9 > ns
    }
    val p2 = forAll(smallInt){ ns =>
      10 > ns
    }

    Prop.run(p1).isInstanceOf[Failed] should be (true)
    Prop.run(p2) == Passed should be (true)
  }

}
