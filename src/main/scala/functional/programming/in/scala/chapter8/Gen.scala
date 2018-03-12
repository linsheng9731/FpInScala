package functional.programming.in.scala.chapter8
import Prop._
import functional.programming.in.scala.chapter5.Stream
import functional.programming.in.scala.chapter6.{RNG, State}
/**
  * Gen
  *
  * @author damon lin
  *         2018/3/12
  */
case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

}


object Gen {
  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  val boolean: Gen[Boolean] =
    Gen(State(RNG.boolean))

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def choose(i: Double, j: Double): Gen[Double] =
    Gen(State(RNG.double).map(d => i + d*(j-i)))

  def even(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if (stopExclusive%2 == 0) stopExclusive - 1 else stopExclusive).
      map (n => if (n%2 != 0) n+1 else n)

  def odd(start: Int, stopExclusive: Int): Gen[Int] =
    choose(start, if (stopExclusive%2 != 0) stopExclusive - 1 else stopExclusive).
      map (n => if (n%2 == 0) n+1 else n)

}


case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop) = Prop (
    (max, n, rng) => run(max, n, rng) match {
      case Passed | Proved => p.run(max, n, rng)
      case x => x
    }
  )

  def ||(p: Prop) = Prop(
    (max, n, rng) => run(max, n, rng) match {
      case Failed(msg, _) => p.tag(msg).run(max, n, rng)
      case x => x
    }
  )

  def tag(msg: String) = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Failed(e, c) => Failed(msg + "\n" + e, c)
      case x => x
    }
  }
}

object Prop {
  type SuccessCount  = Int
  type TestCases  = Int
  type MaxSize  = Int
  type FailedCase = String

  sealed trait Result {
    def isFailed: Boolean
  }

  case object Passed extends Result {
    def isFailed = false
  }

  case class Failed(failure: FailedCase, successCount: SuccessCount) extends Result {
    override def isFailed: Boolean = true
  }

  case object Proved extends Result {
    override def isFailed: Boolean = false
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
    // State[S, +A](run: S => (A, S))
    // unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A]
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
     (max, n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Failed(a.toString, i)
      } catch { case e: Exception => Failed(buildMsg(a, e), i) }
    }.find(x => x.isFailed).getOrElse(Passed)
  }

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Result =
    p.run(maxSize, testCases, rng) match {
      case Failed(msg, n) =>
        println(s"! Failed after $n passed tests:\n $msg")
        Failed(msg, n)
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
        Passed
      case Proved =>
        println(s"+ OK, proved property.")
        Proved
    }

}