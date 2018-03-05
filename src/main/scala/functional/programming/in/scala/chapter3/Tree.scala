package functional.programming.in.scala.chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // size cal total leaf of a tree
  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  }

  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  /*
 Like `foldRight` for lists, `fold` receives a "handler" for each of the data constructors of the type, and recursively
 accumulates some value using these handlers. As with `foldRight`, `fold(t)(Leaf(_))(Branch(_,_)) == t`, and we can use
 this function to implement just about any recursive function that would otherwise be defined by pattern matching.
 */
  def fold[A, B](t:Tree[A])(f: A=> B)(g:(B,B) => B):B = {
    t match {
      case Leaf(v) => f(v)
      case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }

  def sizeViaFold[A](t: Tree[A]): Int = {
    fold(t)(x => 1)(1 + _ + _)
  }

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 0)((d1,d2) => 1 + (d1 max d2))

  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))

}