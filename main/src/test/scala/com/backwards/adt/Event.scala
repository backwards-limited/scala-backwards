package com.backwards.adt

import cats.implicits._
import monocle.macros.Lenses
import com.backwards.adt.Mask._

sealed trait Event {
  def eventDetails: EventDetails

  def activityType: String
}

@Lenses
final case class CreateAccountSuccessEvent(
  eventDetails: EventDetails, personalDetails: PersonalDetails, paymentAccounts: List[PaymentAccounts],
  walletFlag: Boolean, migratedFlag: Boolean, hashedProfileId: String,
  activityType: String = "CREATE_ACCOUNT_SUCCESS"
) extends Event

object CreateAccountSuccessEvent {
  implicit val createAccountSuccessEventMask: Mask[CreateAccountSuccessEvent] =
    event => (mask(personalDetails) andThen mask(paymentAccounts))(event)
}

@Lenses
final case class CreateAccountFailureEvent(
  eventDetails: EventDetails, personalDetails: PersonalDetails, failureDetails: FailureDetails,
  activityType: String = "CREATE_ACCOUNT_FAILURE"
) extends Event

object CreateAccountFailureEvent {
  implicit val createAccountFailureEventMask: Mask[CreateAccountFailureEvent] =
    event => mask(personalDetails).apply(event)
}