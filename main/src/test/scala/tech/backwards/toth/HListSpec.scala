package tech.backwards.toth

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://contramap.dev/posts/2020-02-23-hlist/ HList]]
 */
class HListSpec extends AnyWordSpec with Matchers {
  "List" should {
    "be homegeneous" in {
      // List of Any....... boo
      val xs: List[Any] =
        List(
          "a string",
          true,
          42
        )
    }
  }

  "Tuple" should {
    "be heterogeneous" in {
      val xs: (String, (Boolean, (Int, Unit))) =
        ("a string",
          (true,
            (42, ())
          )
        )
    }
  }

  "HList" should {
    "be heterogeneous" in {
      sealed trait HList

      object HList {
        case object Empty                                extends HList

        case class Cons[A, B <: HList](head: A, tail: B) extends HList
      }

      import HList._

      val xs: Cons[String, Cons[Boolean, Cons[Int, Empty.type]]] =
        Cons("a string",
          Cons(true,
            Cons(42, Empty)
          )
        )
    }

    "be less verbose by adding an operator designed to prepend an element to a HList" in {
      sealed trait HList

      object HList {
        case object Empty                                extends HList

        case class Cons[A, B <: HList](head: A, tail: B) extends HList

        implicit class ops[A <: HList](xs: A) {
          def prepend[B](b: B): Cons[B, A] = Cons(b, xs)
        }
      }

      import HList._

      val xs: Cons[String, Cons[Boolean, Cons[Int, Empty.type]]] =
        Empty.prepend(42)
             .prepend(true)
             .prepend("a string")
    }

    "improve on the less verbose using infix and right associativity" in {
      sealed trait HList

      object HList {
        case object Empty                                extends HList

        case class Cons[A, B <: HList](head: A, tail: B) extends HList

        implicit class ops[A <: HList](a: A) {
          def :*:[B](b: B): Cons[B, A] = Cons(b, a)
        }
      }

      import HList._

      val xs: Cons[String, Cons[Boolean, Cons[Int, Empty.type]]] =
        "a string" :*: true :*: 42 :*: Empty
    }

    "improve the type which is still verbose - try to do the same improvement at the type-level using some type aliases" in {
      sealed trait HList

      object HList {
        case object Empty                                extends HList

        case class Cons[A, B <: HList](head: A, tail: B) extends HList

        // This alias prevents writing Empty.type everywhere we need to refer to `Empty` at the type-level
        type Empty = Empty.type

        type :*:[A, B <: HList] = Cons[A, B]

        implicit class ops[A <: HList](a: A) {
          def :*:[B](b: B): Cons[B, A] = Cons(b, a)
        }
      }

      import HList._

      val xs: String :*: Boolean :*: Int :*: Empty =
        "a string" :*: true :*: 42 :*: Empty
    }

    "what about extracting elements from an HList - use an Extractor" in {
      sealed trait HList

      object HList {
        case object Empty                                extends HList

        case class Cons[A, B <: HList](head: A, tail: B) extends HList

        // This alias prevents writing Empty.type everywhere we need to refer to `Empty` at the type-level
        type Empty = Empty.type

        type :*:[A, B <: HList] = Cons[A, B]

        implicit class ops[A <: HList](a: A) {
          def :*:[B](b: B): Cons[B, A] = Cons(b, a)
        }

        object :*: {
          def unapply[A, B <: HList](cons: Cons[A, B]): Option[(A, B)] =
            Some((cons.head, cons.tail))
        }
      }

      import HList._

      val s :*: b :*: i :*: _ =
        "a string" :*: true :*: 42 :*: Empty
    }

    "concatenate HLists" in {
      sealed trait HList {
        def ++[That <: HList](that: That) /* ? Type ? */

        // No matter the type being returned, we need it to be a subtype of HList
      }
    }

    "concatenate HLists using type level function for type Append and value level function for ++" in {
      sealed trait HList {
        type Append[That <: HList] <: HList

        def ++[That <: HList](that: That): Append[That]
      }

      object HList {
        case object Empty extends HList {
          override type Append[B <: HList] = B

          override def ++[That <: HList](that: That): That = that
        }

        case class Cons[A, B <: HList](head: A, tail: B) extends HList { self =>
          override type Append[That <: HList] = Cons[A, tail.Append[That]]

          override def ++[That <: HList](that: That): self.Append[That] =
            Cons(head, tail ++ that)
        }

        // This alias prevents writing Empty.type everywhere we need to refer to `Empty` at the type-level
        type Empty = Empty.type

        type :*:[A, B <: HList] = Cons[A, B]

        implicit class ops[A <: HList](a: A) {
          def :*:[B](b: B): Cons[B, A] = Cons(b, a)
        }

        object :*: {
          def unapply[A, B <: HList](cons: Cons[A, B]): Option[(A, B)] =
            Some((cons.head, cons.tail))
        }
      }

      import HList._

      val string :*: bool :*: int :*: _ =
        "a string" :*: true :*: 42 :*: Empty

      val x = "a string" :*: Empty
      val y = 42 :*: Empty

      val s :*: i :*: _ =
        x ++ y
    }
  }
}