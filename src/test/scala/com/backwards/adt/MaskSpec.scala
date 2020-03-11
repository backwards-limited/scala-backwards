package com.backwards.adt

import pprint._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.adt.Mask._
import com.backwards.adt.Mask.ops._

class MaskSpec extends AnyWordSpec with Matchers with MaskFixture {
  "Mask" should {
    "obfuscate names, emails and phone numbers of a 'create account success event'" in {
      val event: CreateAccountSuccessEvent =
        CreateAccountSuccessEvent(eventDetails, personalDetails, List(paymentAccounts), walletFlag = true, migratedFlag = true, "hashedAccountId")

      val maskedEvent: CreateAccountSuccessEvent = event.mask

      pprintln(maskedEvent)

      List(
        maskedEvent.personalDetails.email,
        maskedEvent.personalDetails.firstName.get,
        maskedEvent.personalDetails.lastName.get,
        maskedEvent.personalDetails.mobile.get,
        maskedEvent.paymentAccounts.head.signupEmail
      ).foreach(_ mustBe masking)
    }
  }
}

trait MaskFixture {
  val eventDetails: EventDetails =
    EventDetails("PA", "TV", "GB", "TV", "PA", "654321")

  val personalDetails: PersonalDetails =
    PersonalDetails("blah@gmail.com", Option("Mr"), Option("John"), Option("Doe"), Option("07777777777"), Option(true), Option(true))

  val paymentAccounts: PaymentAccounts =
    PaymentAccounts("654321", "CARD", "PA", "VISA", "1234", "12/19", "654321", "blah@gmail.com")

}