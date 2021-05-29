package com.backwards.fp.monad

import scala.annotation.tailrec

object ListOps {
  import com.backwards.fp.functor.ListOps._

  implicit val listMonad: Monad[List] = new Monad[List] {
    override def pure[A](a: A): List[A] = List(a)

    override def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
      @tailrec
      def go(as: List[A], acc: List[B]): List[B] = (as, acc) match {
        case (Nil, bs) => bs
        case (h +: t, bs) => go(t, bs ++ f(h))
        case _ => sys.error("Whoops")
      }

      go(as, List.empty[B])
    }
  }
}