package tech.backwards.adt

import java.time.LocalDate

final case class EventDetails(
  originatingSystem: String, provider: String,
  providerTerritory: String, proposition: String, partnerId: String, customerPartnerAccountId: String,
  householdId: Option[String] = None, activityTimestamp: LocalDate = LocalDate.now()
)