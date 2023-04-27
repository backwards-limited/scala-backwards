package tech.backwards.fp.learn

trait TypeClass[A] {
  def foo(a: A): String
}

object TypeClass { self =>
  def apply[A: TypeClass]: TypeClass[A] =
    implicitly

  def foo[A: TypeClass](a: A): String =
    apply[A].foo(a)

  object syntax {
    implicit class TypeClassSyntax[A: TypeClass](a: A) {
      lazy val foo: String =
        self.foo(a)
    }
  }

  implicit val typeClassInt: TypeClass[Int] =
    (a: Int) => s"Int: $a"

  implicit val typeClassString: TypeClass[String] =
    (a: String) => s"String: $a"
}