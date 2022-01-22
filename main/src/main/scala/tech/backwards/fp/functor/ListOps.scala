package tech.backwards.fp.functor

object ListOps {
  implicit val listFunctor: Functor[List] =
    new Functor[List] {
      def fmap[A, B](fa: List[A])(f: A => B): List[B] = {
        lazy val go: List[A] => List[B] = {
          case Nil => Nil
          case h +: t => f(h) +: go(t)
          case _ => sys.error("Whoops")
        }

        go(fa)
      }
    }
}