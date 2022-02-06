package tech.backwards.typelevel

import shapeless._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://automateddeveloper.blogspot.com/2020/03/re-thinking-visitor-pattern-with-scala.html RE-THINKING THE VISITOR PATTERN WITH SCALA, SHAPELESS & POLYMORPHIC FUNCTIONS]]
 */
class VisitorPatternSpec extends AnyWordSpec with Matchers {
  sealed trait Element

  sealed trait SimpleElement[A] extends Element {
    def value: A
  }

  case class ComplexElement(value: List[Element]) extends Element

  case class TextElement(value: String) extends SimpleElement[String]

  case class NumberElement(value: Double) extends SimpleElement[Double]

  case class BooleanElement(value: Boolean) extends SimpleElement[Boolean]

  "Validator Pattern (for a tree structure such as XML)" should {
    "apply first iteration with type class" in {
      sealed trait ValidatorTypeClass[A] {
        def validate(a: A): Boolean
      }

      object ValidatorTypeClass {
        implicit val validatorTypeClassString: ValidatorTypeClass[String] =
          new ValidatorTypeClass[String] {
            override def validate(a: String): Boolean = true // Validation logic for strings
          }

        implicit val validatorTypeClassDouble: ValidatorTypeClass[Double] =
          new ValidatorTypeClass[Double] {
            override def validate(a: Double): Boolean = ??? // Validation logic for numbers
          }

        implicit val validatorTypeClassBoolean: ValidatorTypeClass[Boolean] =
          new ValidatorTypeClass[Boolean] {
            override def validate(a: Boolean): Boolean = ??? // Validation logic for booleans
          }

        implicit val validatorTypeClassComplexElement: ValidatorTypeClass[ComplexElement] =
          new ValidatorTypeClass[ComplexElement] {
            override def validate(a: ComplexElement): Boolean =
              a.value.forall(validateElement)
          }

        // validatorTypeClassComplexElement only compiles with the following type class instance for Element
        // However, the following kind of defeats the purpose of a type class, as we end up with a giant pattern match to delegate:
        implicit val validatorTypeClassElement: ValidatorTypeClass[Element] =
          new ValidatorTypeClass[Element] {
            def validate(a: Element): Boolean =
              // Pattern match every type of Element
              a match {
                case TextElement(s) =>
                  validatorTypeClassString.validate(s)

                case _ =>
                  ???
              }
          }

        def validateElement[A: ValidatorTypeClass](a: A): Boolean =
          implicitly[ValidatorTypeClass[A]].validate(a)
      }

      import ValidatorTypeClass._

      val complex: ComplexElement =
        ComplexElement(
          List(
            TextElement(value = "first element"),
            TextElement(value = "second element")
          )
        )

      val result: Boolean =
        validateElement(complex)

      println(s"1: $result")
    }

    "apply second iteration with help from Shapeless" in {
      sealed trait ValidatorTypeClass[A] {
        def validate(a: A): Boolean
      }

      /*
      Function "everywhere" allows in place editing of tree like structures (or any structures really).

      Function "everything" instead of editing, lets you simply traverse, or visit generic data structures.
      "everything" takes three arguments:
      everything(validates)(combine)(complex)

      - the first one is a polymorphic function that we want to process every step of the data structure
      - combine is a polymorphic function to combine the results
      - complex (the third argument above) is our input - in this case the root of our nested data model.
      */

      /*
      Lets start with our polymorphic function for validating every step (this will be every attribute on each class, including lists, maps and other classes that will then get traversed as well.
      */
      sealed trait DefaultValidation extends Poly1 {
        implicit def default[A]: Case.Aux[A, Boolean] =
          at[A](a => true)
      }

      object validates extends DefaultValidation {
        /*
        As the cases in this polymorphic function are implicits, we need to have the default case in the parent (DefaultValidation) so it is resolved as a lower priority than our validating implicit.
        */
        implicit def caseValidated[A](implicit v: ValidatorTypeClass[A]): Case.Aux[A, Boolean] =
          at[A](a => v.validate(a))
      }

      object combine extends Poly2 {
        implicit def caseValidation: Case.Aux[Boolean, Boolean, Boolean] =
          at[Boolean, Boolean](_ && _)
      }

      object ValidatorTypeClass {
        implicit val validatorTypeClassString: ValidatorTypeClass[String] =
          new ValidatorTypeClass[String] {
            override def validate(a: String): Boolean = true // Validation logic for strings
          }

        implicit val validatorTypeClassDouble: ValidatorTypeClass[Double] =
          new ValidatorTypeClass[Double] {
            override def validate(a: Double): Boolean = ??? // Validation logic for numbers
          }

        implicit val validatorTypeClassBoolean: ValidatorTypeClass[Boolean] =
          new ValidatorTypeClass[Boolean] {
            override def validate(a: Boolean): Boolean = ??? // Validation logic for booleans
          }

        implicit val validatorTypeClassComplexElement: ValidatorTypeClass[ComplexElement] =
          new ValidatorTypeClass[ComplexElement] {
            override def validate(a: ComplexElement): Boolean =
              true // everything(validates)(combine)(a)
          }

        // No longer required
        // implicit val validatorTypeClassElement: ValidatorTypeClass[Element] =
      }

      val complex: ComplexElement =
        ComplexElement(
          List(
            TextElement(value = "first element"),
            TextElement(value = "second element")
          )
        )

      // HA! Get a compiler error
      /*val result: Boolean =
        everything(validates)(combine)(complex)

      println(s"2: $result")*/

      /*
      If we were to have this perform validation, rather than return booleans, we could look to use something like Validated from Cats.
      This would allow us to accumulate meaningful failures throughout the traversal.
      This is really simple to drop in, and all we would need to do is implement the combine polymorphic function for ValidatedNel class:

      object combineFieldValues extends Poly2 {
        implicit def caseValidation =
          at[ValidatedNel[String, Boolean], ValidatedNel[String, Boolean]] ({
            case (a, b) => a.combine(b)
          })
      */
    }
  }
}