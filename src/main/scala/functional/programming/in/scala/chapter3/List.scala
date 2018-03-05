package functional.programming.in.scala.chapter3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head:A, tail:List[A]) extends List[A] {

  override def toString: String = {
    this match {
      case Cons(x, xs) => if(xs != Nil) x.toString +","+ xs.toString else x.toString
      case _ => ""
    }
  }
}

object List {

  def head[A](l:List[A]):A = l match {
    case Nil => throw new NoSuchElementException("head of empty list")
    case Cons(x, xs) => x
  }

  // append element to head of list
  def append[A](x: A, l:List[A]):List[A] = {
    l match {
      case Nil => List(x)
      case c:Cons[A] => Cons(x, c)
    }
  }

  // append element to tail of list
  def append[A](l:List[A], x: A):List[A] = {
    if(l==Nil)  {
      Cons(x, Nil)
    } else {
      Cons(head(l), append(tail(l), x))
    }
  }

  // merge two list into one
  def merge[A](l1: List[A], l2:List[A]): List[A] = {
    l1 match {
      case Nil => l1
      case Cons(h,t) => Cons(h, merge(t, l2))
    }
  }

  // generalizing to higher-order function to avoid duplication
  def foldRight[A, B](l: List[A], z:B)(f:(A,B) => B):B = {
    l match {
      case Nil => z
      case Cons(x, xs) =>
        // eg for sum function:
        // f(x0, f(x1, f(x2, z))) => f(1, f(2, f(3, 0))) => (((3 + 0) + 2) + 1)
        f(x, foldRight(xs,z)(f))
    }
  }

  def sum(l: List[Int]):Int = foldRight(l, 0)(_ + _)

  def product(l: List[Double]): Double = foldRight(l, 1.0)(_ * _)

  def tail[A](l: List[A]) : List[A]= {
    l match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }
  }

  def drop[A](n:Int, l: List[A]): List[A] = {
    var count = n
    var these = l
    while(count > 0 && these!=Nil) {
     these = tail(these)
     count -= 1
    }
    these
  }

  def takeWhile[A](l:List[A])(p: A=> Boolean):List[A] = {
    var these  = l
    var b = List[A]()
    while(these != Nil && p(head(these))) {
      val h = head(these)
      b = append(b, h)
      these = tail(these)
    }
    b
  }

  def apply[A](as: A*):List[A] = {
    if(as.isEmpty) {
      Nil
    } else{
      Cons(as.head, apply(as.tail:_*))
    }
  }
}

