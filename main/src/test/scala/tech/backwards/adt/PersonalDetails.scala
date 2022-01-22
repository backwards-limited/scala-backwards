package tech.backwards.adt

import cats.implicits._
import monocle.macros.Lenses
import tech.backwards.adt.Mask._

@Lenses
final case class PersonalDetails(
  email: String,
  title: Option[String] = None,
  firstName: Option[String] = None,
  lastName: Option[String] = None,
  mobile: Option[String] = None,
  terms: Option[Boolean] = None,
  marketingConsent: Option[Boolean] = None
)

object PersonalDetails {
  implicit val personalDetailsMask: Mask[PersonalDetails] =
    p => (mask(email) andThen mask(title) andThen mask(firstName) andThen mask(lastName) andThen mask(mobile))(p)
}