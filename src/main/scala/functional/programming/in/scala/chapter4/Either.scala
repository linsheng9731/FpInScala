package functional.programming.in.scala.chapter4

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] =  {
    this match {
      case Right(e) => Right(f(e))
      case Left(e) => Left(e)
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(e) => f(e)
      case Left(e) => Left(e)
    }
  }

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def mean(xs: Seq[Double]) : Either[String, Double] = {
    if(xs.isEmpty){
      Left("mean of empty list!")
    } else {
      Right(xs.sum / xs.length)
    }
  }

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = {
    try {
      Right(x / y)
    } catch {
      case e: Exception => Left(e)
    }
  }

}