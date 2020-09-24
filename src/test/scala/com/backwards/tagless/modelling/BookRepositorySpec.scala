package com.backwards.tagless.modelling

import cats.Id
import cats.implicits._
import monocle.macros.GenLens
import monocle.std.option._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.fp.Function.syntax._

class BookRepositorySpec extends AnyWordSpec with Matchers {
  val repository = new InMemoryBookRepository

  val book: Book = Book(
    id = Some(BookId("1")),
    title = "A Game of Thrones",
    author = "George R. R. Martin"
  )

  "Book repository" should {
    "retrieve a book" in {
      val retrievedBook: Id[Option[Book]] = for {
        _ <- repository addBook book
        bookId <- book.id.pure[Id]
        retrievedBook <- repository getBook bookId
      } yield retrievedBook

      retrievedBook mustBe Option(book)
    }

    "not retrieve a book because of incorrect book id" in {
      val retrievedBook: Id[Option[Book]] = for {
        _ <- repository addBook book
        bookId <- Option(BookId("wrong id")).pure[Id]
        retrievedBook <- repository getBook bookId
      } yield retrievedBook

      retrievedBook mustBe None
    }

    "not retrieve a book because no book id" in {
      val retrievedBook: Id[Option[Book]] = for {
        _ <- repository addBook book
        bookId <- None.pure[Id]
        retrievedBook <- repository getBook bookId
      } yield retrievedBook

      retrievedBook mustBe None
    }

    "list all added books" in {
      val bookSequel = sequel(book)

      val listedBooks: Id[Iterable[Book]] = for {
        _ <- repository addBook book
        _ <- repository addBook bookSequel
        books <- repository.listBooks
      } yield books

      listedBooks.toSeq must contain inOrder(book, bookSequel)
    }
  }

  def sequel(book: Book): Book = {
    val bookId: String => BookId = BookId.apply

    val incrementBookId: String => BookId =
      _ + ".2" |> bookId

    val bookIdLens = GenLens[Book](_.id).composePrism(some).modify(_.id |> incrementBookId)
    val titleLens = GenLens[Book](_.title).modify(_ + " The Sequel")

    (bookIdLens compose titleLens)(book)
  }
}