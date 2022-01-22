package tech.backwards.fp.foldable

object ListOps {
  implicit val listFoldable: Foldable[List] =
    new Foldable[List] {
      def foldr[A, B](fa: List[A])(acc: B)(f: (A, B) => B): B = {
        lazy val go: (List[A], B) => B = {
          case (Nil, b) => b
          case (h +: t, b) => go(t, f(h, b))
          case _ => sys.error("Whoops")
        }

        go(fa.reverse, acc)
      }
    }
}
