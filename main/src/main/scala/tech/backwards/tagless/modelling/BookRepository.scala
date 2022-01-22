package tech.backwards.tagless.modelling

import cats.Id

/**
  * The book repository defines an algebra for manipulating books
  */
trait BookRepository[F[_]] {
  /** Get the list of all books */
  def listBooks: F[Iterable[Book]]

  /** Get a book based on its id. Resulting in None if the book doesn't exist*/
  def getBook(id: BookId): F[Option[Book]]

  /** Add a a book */
  def addBook(book: Book): F[Unit]
}

/**
  * A BookRepository with effect Id
  */
class InMemoryBookRepository extends BookRepository[Id] {
  private var books =
    Map.empty[BookId, Book]

  def listBooks: Id[Iterable[Book]] =
    books.values

  def getBook(id: BookId): Id[Option[Book]] =
    books get id

  def addBook(book: Book): Id[Unit] =
    book.id.foreach { bookId =>
      books = books + (bookId -> book)
    }
}