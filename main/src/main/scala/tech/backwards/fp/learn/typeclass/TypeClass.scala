package tech.backwards.fp.learn.typeclass

trait TypeClass[A] {
  def foo(a: A): String
}

object TypeClass extends TypeClassImplicits {
  def apply[A: TypeClass]: TypeClass[A] = implicitly

  def foo[A: TypeClass](a: A): String =
    TypeClass[A].foo(a)

  object syntax {
    implicit class TypeClassOps[A: TypeClass](a: A) {
      lazy val foo: String =
        TypeClass[A].foo(a)
    }
  }
}

trait TypeClassImplicits {
  implicit val typeClassInt: TypeClass[Int] =
    (a: Int) => s"Int: $a"

  implicit val typeClassString: TypeClass[String] =
    (a: String) => s"String: $a"
}