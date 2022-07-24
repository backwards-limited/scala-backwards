package tech.backwards.fp.laws

import scala.util.chaining.scalaUtilChainingOps
import play.api.libs.json._
import cats.Applicative
import cats.implicits._
import cats.kernel.Eq
import cats.laws._
import cats.laws.discipline._
import munit.{DisciplineSuite, ScalaCheckSuite}
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen}
import org.typelevel.discipline.Laws

/**
 * [[https://www.freecodecamp.org/news/an-introduction-to-law-testing-in-scala-4243d72272f9/ Introduction to Law testing in Scala]]
 */
class PropertyBasedLawSuite extends ScalaCheckSuite {
  val personFormat: OFormat[Person] =
    new OFormat[Person] {
      def reads(json: JsValue): JsResult[Person] =
        for {
          name  <- (json \ "name").validate[String]
          age   <- (json \ "age").validate[Int]
        } yield Person(name, age)

      def writes(p: Person): JsObject =
        JsObject(List("name" -> JsString(p.name), "age" -> JsNumber(p.age)))
    }

  test("Serialise and deserialise a person") {
    val person: Person =
      Person("Bob", 23)

    val json: JsObject =
      personFormat.writes(person)

    val JsSuccess(deserialisedPerson: Person, _) =
      personFormat.reads(json)

    assertEquals(deserialisedPerson, person)
  }
  /*
  We now need to ask ourselves, whether all people will serialize successfully?
  What about a person with invalid data (such as negative age)?
  Will we want to repeat this thought process of finding edge-cases for all our test data?

  And most importantly, will this code remain readable over time?
  e.g. changing the person data type [adding a LastName field], repeated tests for other data types, etc.

  We can solve any problem by introducing an extra level of indirection... Property based testing, and laws testing.
  */

  implicit val personArbitrary: Arbitrary[Person] =
    Arbitrary(
      Gen.alphaStr.flatMap(name =>
        Gen.chooseNum(0, 120).map(age =>
          Person(name, age)
        )
      )
    )

  property("Person codec round trip") {
    forAll((p: Person) =>
      assertEquals(
        personFormat.reads(personFormat.writes(p.tap(println))).asOpt,
        p.some
      )
    )
  }
}

class PropertyBasedLawMagnolifySuite extends ScalaCheckSuite {
  import magnolify.scalacheck.auto._

  val personFormat: OFormat[Person] =
    new OFormat[Person] {
      def reads(json: JsValue): JsResult[Person] =
        for {
          name  <- (json \ "name").validate[String]
          age   <- (json \ "age").validate[Int]
        } yield Person(name, age)

      def writes(p: Person): JsObject =
        JsObject(List("name" -> JsString(p.name), "age" -> JsNumber(p.age)))
    }

  property("Person codec round trip") {
    forAll((p: Person) =>
      assertEquals(
        personFormat.reads(personFormat.writes(p.tap(println))).asOpt,
        p.some
      )
    )
  }
  /*
  We abstract further by defining laws of our data type and claim that A and B are isomorphic:
  */
  trait CodecLaws[A, B] {
    val serialize: A => B

    val deserialize: B => A

    val codecRoundTrip: A => Boolean =
      a => (serialize andThen deserialize)(a) == a
  }
}

/*
Nicer to use IsEq from cats-laws library in conjunction with Discipline:
*/
class CodecLaws[F[_]: Applicative, A, B](val serialize: A => B, val deserialize: B => F[A]) {
  val codecRoundTrip: A => IsEq[F[A]] =
    a => (serialize andThen deserialize)(a) <-> a.pure[F]
}

abstract class CodecTests[F[_]: Applicative, A, B] extends Laws {
  def laws: CodecLaws[F, A, B]

  def tests(implicit arbitrary: Arbitrary[A], eqA: cats.Eq[F[A]]): RuleSet =
    new DefaultRuleSet(
      name   = "name",
      parent = None,
      "roundTrip" -> forAll(laws.codecRoundTrip)
      /*
      Shortcut for:
      "roundTrip" -> forAll((a: A) =>
        laws.codecRoundTrip(a)
      )*/
    )
}

object JsonCodecTests {
  implicit val applicativeJsResult: Applicative[JsResult] =
    new Applicative[JsResult] {
      override def pure[A](x: A): JsResult[A] =
        play.api.libs.json.JsResult.applicativeJsResult.pure(x)

      override def ap[A, B](ff: JsResult[A => B])(fa: JsResult[A]): JsResult[B] =
        play.api.libs.json.JsResult.applicativeJsResult(ff, fa)
    }

  def apply[A: Arbitrary](implicit format: OFormat[A]): CodecTests[JsResult, A, JsValue] =
    new CodecTests[JsResult, A, JsValue] {
      override def laws: CodecLaws[JsResult, A, JsValue] =
        new CodecLaws[JsResult, A, JsValue](format.writes, format.reads)
    }
}

class CodecSuite extends DisciplineSuite {
  import magnolify.scalacheck.auto._

  implicit def eq[T]: Eq[T] =
    Eq.fromUniversalEquals

  implicit val personFormat: OFormat[Person] =
    new OFormat[Person] {
      def reads(json: JsValue): JsResult[Person] =
        for {
          name  <- (json \ "name").validate[String]
          age   <- (json \ "age").validate[Int]
        } yield Person(name, age)

      def writes(p: Person): JsObject =
        JsObject(List("name" -> JsString(p.name), "age" -> JsNumber(p.age)))
    }

  checkAll("CodecSuite", JsonCodecTests[Person].tests)
}

final case class Person(name: String, age: Int)