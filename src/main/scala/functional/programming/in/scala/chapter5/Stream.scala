package functional.programming.in.scala.chapter5

import Stream._
sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  /*
  This solution will stack overflow for large
  streams since it's not a tail-recursive
  */
  def toListRecur: List[A] = this match {
    case Cons(h, t) => h() :: t().toListRecur
    case _ => List()
  }

  /*
  A tailrec solution, use a list buffer to
  avoid reverse operation
  */
  def toList: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]

    @annotation.tailrec
    def go(s: Stream[A]) : List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        go(t())
      case _ =>
        buf.toList
    }
    go(this)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, t) if n == 1 => cons(h(), empty) // end
    case _ => empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this // end
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: ()=> Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]) : Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def constant[A](a: A): Stream[A] = {
    lazy val t: Stream[A] = Cons(() => a, () => t)
    t
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n+1))
  }

  def fibs(l: Int, r: Int): Stream[Int] = {
    // 1 1 2 3 5
    if (l==1 && r ==1) {
      cons(1, fibs(1, 2))
    } else {
      cons(l, fibs(r, l + r))
    }
  }

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

}
