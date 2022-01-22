package tech.backwards.tagless.modelling

import cats.Id

/**
  * The user repository defines an algebra for manipulating users
  */
trait UserRepository[F[_]] {
  /** Lookup a user based on unique id */
  def getUser(id: UserId): F[Option[User]]

  /** Add a new user */
  def addUser(user: User): F[Unit]

  /** Update a user */
  def updateUser(user: User): F[Unit]
}

/**
  * A UserRepository with effect Id
  */
class InMemoryUserRepository extends UserRepository[Id] {
  private var users =
    Map.empty[UserId, User]

  def getUser(id: UserId): Id[Option[User]] =
    users get id

  def addUser(user: User): Id[Unit] =
    user.id.foreach { userId =>
      users = users + (userId -> user)
    }

  def updateUser(user: User): Id[Unit] =
    addUser(user)
}