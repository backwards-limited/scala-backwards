package com.backwards.tagless.modelling

/**
  * The application allow users to add and remove books from their reading list.
  */
final case class UserId(id: String) extends AnyVal

/**
  * Represents a user and the list of books on their reading list.
  */
final case class User(
  id: Option[UserId], // Unique id of a user
  firstName: String,  // User's first name
  lastName: String,   // User's last name
  books: List[BookId] // Books the user has on their reading list
)

final case class BookId(id: String) extends AnyVal

/**
  * Represents the details of a book.
  */
final case class Book(
  id: Option[BookId], // Unique id of the book
  title: String,      // Book's title
  author: String      // Author of the book
)

/**
  * Represents the combination of a user and the book details
  */
final case class ReadingList(user: User, books: List[Book])