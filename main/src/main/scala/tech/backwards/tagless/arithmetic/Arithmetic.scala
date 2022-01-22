package tech.backwards.tagless.arithmetic

/**
 * Tagless final can be thought of as one way to encode the interpreter pattern.
 */
object Arithmetic {
  /**
   * First/Standard way to implement our Arithmetic, is to use ADTs and pattern matching
   */
  object ADT {
    sealed abstract class Exp

    case class Lit(value: Int) extends Exp // Constructs a new integer literal

    case class Add(lhs: Exp, rhs: Exp) extends Exp // Sums two integer expressions

    case class Sub(lhs: Exp, rhs: Exp) extends Exp // Subtracts two integer expressions

    case class Mul(lhs: Exp, rhs: Exp) extends Exp // Multiplies two integer expressions

    val evaluate: Exp => Int = {
      case Lit(value) => value
      case Add(lhs, rhs) => evaluate(lhs) + evaluate(rhs)
      case Sub(lhs, rhs) => evaluate(lhs) - evaluate(rhs)
      case Mul(lhs, rhs) => evaluate(lhs) * evaluate(rhs)
    }
  }

  /**
   * To translate our ADT representation of the expression language into the tagless final encoding:
   * 1. Make each case class a method on a trait, with a type parameter.
   * 2. Any parameters to the case classes become parameters to the methods.
   * 3. Anywhere the base type of our ADT (ADT.Exp in this case) is used, we use the type parameter.
   */
  object Tagless {
    trait Exp[A] {
      def lit(value: Int): A

      def add(lhs: A, rhs: A): A

      def sub(lhs: A, rhs: A): A

      def mul(lhs: A, rhs: A): A
    }

    /**
     * Define an evaluator for the tagless final encoding of our expression language.
     * Note that the evaluator becomes an implementation of the trait that defines the operations in the language,
     * rather than a pattern match on the case classes.
     */
    object Evaluate extends Exp[Int] {
      override def lit(value: Int): Int = value

      override def add(lhs: Int, rhs: Int): Int = lhs + rhs

      override def sub(lhs: Int, rhs: Int): Int = lhs - rhs

      override def mul(lhs: Int, rhs: Int): Int = lhs * rhs
    }
  }

  /**
   * The evaluator function for ADT.Exp that is implemented in terms of Tagless.Exp
   */
  def translate[A](adtExp: ADT.Exp, taglessExp: Tagless.Exp[A]): A =
    adtExp match {
      case ADT.Lit(v) => taglessExp.lit(v)
      case ADT.Add(lhs, rhs) => taglessExp.add(translate(lhs, taglessExp), translate(rhs, taglessExp))
      case ADT.Sub(lhs, rhs) => taglessExp.sub(translate(lhs, taglessExp), translate(rhs, taglessExp))
      case ADT.Mul(lhs, rhs) => taglessExp.mul(translate(lhs, taglessExp), translate(rhs, taglessExp))
    }

  /**
   * The evaluator for Tagless.Exp that is implemented in terms of ADT.Exp
   */
  object Reify extends Tagless.Exp[ADT.Exp] {
    override def lit(value: Int): ADT.Exp =
      ADT.Lit(value)

    override def add(lhs: ADT.Exp, rhs: ADT.Exp): ADT.Exp =
      ADT.Add(lhs, rhs)

    override def sub(lhs: ADT.Exp, rhs: ADT.Exp): ADT.Exp =
      ADT.Sub(lhs, rhs)

    override def mul(lhs: ADT.Exp, rhs: ADT.Exp): ADT.Exp =
      ADT.Mul(lhs, rhs)
  }
}