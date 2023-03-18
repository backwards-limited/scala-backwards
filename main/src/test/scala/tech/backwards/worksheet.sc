import cats._
import cats.implicits._

val v1: String Either Option[Int] =
  Traverse[Option].traverse(none[Int])(_ => "a".asLeft[Int])

val v2: Option[String Either Int] =
  Traverse[String Either *].traverse("a".asLeft[Int])(_ => none[Int])