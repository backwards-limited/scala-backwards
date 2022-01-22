package tech.backwards.adt

import cats.implicits._
import monocle.macros.Lenses
import tech.backwards.adt.Mask._

@Lenses
final case class PaymentAccounts(
  cardAlias: String,
  paymentMethodType: String,
  paymentProvider: String,
  paymentMethod: String,
  lastDigits: String,
  expiryDate: String,
  customerReference: String,
  signupEmail: String
)

object PaymentAccounts {
  implicit val paymentAccountsMask: Mask[PaymentAccounts] =
    p => mask(signupEmail)(p)
}