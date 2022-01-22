import cats.Functor
import cats.implicits._

final case class MyType[A, B](a: A, b: B)

// Following is fine - Just doesn't work in worksheet
/*
implicit def myTypeFunctor[X] = new Functor[MyType[X, *]] {
  def map[A, B](fa: MyType[X, A])(f: A => B): MyType[X, B] = ???
}
*/

// Lambda version:
implicit def myTypeFunctorL[X] = new Functor[({ type Y[A] = MyType[X, A] })# Y] {
  def map[A, B](fa: MyType[X, A])(f: A => B): MyType[X, B] = ???
}