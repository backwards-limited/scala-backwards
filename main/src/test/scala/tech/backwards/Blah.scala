package tech.backwards

import cats.Id
import cats.implicits._
import cats.data.{EitherT, IdT}

object Blah extends App {
  val e: EitherT[Option, String, Int] =
    EitherT.pure[Option, String](10)

  println(e.value) // Some(Right("foo"))

  //10.pure[IdT[Id, *]]

  val i: IdT[Option, Int] =
    IdT.pure[Option, Int](10)

  println(i.value) // Some(10)
}
