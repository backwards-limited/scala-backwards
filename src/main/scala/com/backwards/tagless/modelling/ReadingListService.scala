package com.backwards.tagless.modelling

import scala.language.higherKinds

/**
  * The reading list service defines the core domain operations of our application.
  */
trait ReadingListService[F[_]] {
  def getReadingList(user: UserId): F[ReadingList]

  def addToReadingList(userId: UserId, bookId: BookId): F[Unit]

  def removeFromReadingList(userId: UserId, bookId: BookId): F[Unit]
}