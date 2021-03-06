package functional.programming.in.scala.chapter4

trait Option[+A] {

  def map[B](f: A => B): Option[B] = {
     this match {
       case Some(a) => Some(f(a))
       case None => None
     }
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case Some(a) => a
      case None => default
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this.map(f).getOrElse(None)
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this match {
      case None =>  ob
      case _ => this
    }
  }

  def filter(f: A => Boolean): Option[A] = {
    this match {
      case Some(a) => if(f(a)) Some(a) else None
      case None => None
    }
  }

}

case class Some[A](v: A) extends Option[A]
case object None extends Option[Nothing]