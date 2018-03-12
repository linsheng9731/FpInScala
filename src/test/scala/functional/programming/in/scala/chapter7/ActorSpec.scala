package functional.programming.in.scala.chapter7

import org.scalatest.{FlatSpec, Matchers}
import java.util.concurrent._
import language.implicitConversions

/**
  * ActorSpec
  *
  * @author damon lin
  *         2018/3/10
  */
class ActorSpec extends FlatSpec with Matchers {


  "actor" should "run handler function async" in {

    def sum(l: List[Int]): Unit = {
      val ret = l.foldLeft(0)((l,r) => l + r)
      println(ret)
    }

    val s = new ForkJoinPool()
    val actor = Actor(s)(sum)
    actor ! List(1,2,3,4)
  }

}
