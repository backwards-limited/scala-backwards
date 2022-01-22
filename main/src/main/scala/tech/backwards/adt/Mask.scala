package tech.backwards.adt

import cats.Functor
import cats.implicits._
import monocle.Lens
import shapeless._

trait Mask[A] extends {
  def apply(a: A): A
}

object Mask extends Masking {
  val masking = "*****" // TODO - configurable - maybe pass into "mask" function and use this as the default.

  def apply[A: Mask](a: A): A = implicitly[Mask[A]].apply(a)

  def mask[M](lens: Lens[M, String]): M => M =
    lens.modify(_ => masking)

  def mask[M, F[_]: Functor](lens: Lens[M, F[String]]): M => M =
    lens.modify(_.map(_ => masking))

  def mask[M, A: Mask](lens: Lens[M, A]): M => M =
    lens.modify(apply[A])

  def mask[M, F[_]: Functor, A: Mask](lens: Lens[M, F[A]]): M => M =
    lens.modify(_.map(apply[A]))

  object ops {
    implicit class MaskOps[M: Mask](m: M) {
      def mask: M = apply(m)
    }
  }
}

trait Masking {
  implicit val hnilMask: Mask[HNil] =
    _ => HNil

  implicit def hListMask[H, T <: HList](
    implicit MaskH: Mask[H], MaskT: Lazy[Mask[T]]
  ): Mask[H :: T] = {
    case h :: t => MaskH(h) :: MaskT.value(t)
  }

  implicit val cnilMask: Mask[CNil] =
    _ => throw new Exception("Inconceivable")

  implicit def coproductMask[A, B <: Coproduct](
    implicit MaskA: Lazy[Mask[A]], MaskB: Lazy[Mask[B]]
  ): Mask[A :+: B] = {
    case Inl(a) => Inl(MaskA.value(a))
    case Inr(b) => Inr(MaskB.value(b))
  }

  implicit def genMask[A, R](
    implicit G: Generic.Aux[A, R], M: Lazy[Mask[R]]
  ): Mask[A] =
    a => G.from(M.value(G.to(a)))
}