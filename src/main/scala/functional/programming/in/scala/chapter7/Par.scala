package functional.programming.in.scala.chapter7

import java.util.concurrent._
import language.implicitConversions

object Par {

  // define a function type
  type Par[A] = (ExecutorService) => Future[A]

  // a is a function, so a(s) means the function call with a ExecutorService parameter
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  // wrap a into a Par which is a function type
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  // wrap get into a Future
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  // input two Par function output a new Par function
  // es is ExecutorService type, the compiler can cal with Par definition:
  // (ExecutorService) => Future[A]
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    es => {
      val af = run(es)(a) // get result
      val bf = run(es)(b)
      UnitFuture(f(af.get, bf.get)) // f(a, b) return new result c
    }

  // 'a' is a call by name parameter
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call(): A = a(es).get
    })

  // wrap a into a function to use thread
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  // use map2 build map
  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  // _.sorted is function f, parList is parameter, so result is Par[sortedList]
  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  // List => Par
  def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
    as match {
      case Nil => unit(Nil)
      case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
    }

  // Seq => Par
  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l,r) = as.splitAt(as.length/2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  // List => Par (combine map and sequenceBalanced)
  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  // List => Par
  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      l map (asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(pars))(_.flatten) // convenience method on `List` for concatenating a list of lists
  }

}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }
}
