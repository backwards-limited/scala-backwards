package tech.backwards.optics

import monocle.{POptional, Prism}
import monocle.macros.GenLens
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PrismSpec extends AnyWordSpec with Matchers {
  "Prism" should {
    "be demonstrated" in {
      /*
      Why is Prism named thus?

      It works on Sum types (coproducts) which form a hierarchy e.g.

                Shape
             /    |    \
            /     |     \
      Circle  Rectangle  Triangle
      */

      sealed trait Shape

      final case class Circle(radius: Double) extends Shape

      final case class Rectangle(width: Double, height: Double) extends Shape

      final case class Triangle(a: Double, b: Double, c: Double) extends Shape

      val circle: Circle = Circle(4.0)
      val rectangle: Rectangle = Rectangle(3.0, 5.0)
      val triangle: Triangle = Triangle(3, 4, 5)

      val shape: Shape = circle

      // To polymorphically work with a Circle as a Shape:
      // Shape -> Double (Radius)
      // Double (Radius) -> Shape
      val circlePrism: Prism[Shape, Double] =
        Prism.partial[Shape, Double] { case Circle(r) => r } (Circle)

      val anotherCircle: Shape =
        circlePrism(7.0) // Think of this as a smart constructor

      val radius: Option[Double] =
        circlePrism.getOption(circle)

      radius mustBe Option(circle.radius)

      val noRadius: Option[Double] =
        circlePrism.getOption(rectangle)

      noRadius mustBe None


      // We can combine Prisms with Lenses

      final case class Icon(background: String, shape: Shape)

      final case class Logo(colour: String)

      final case class BrandIdentity(logo: Logo, icon: Icon)

      // Change the radius of an icon of a brand

      val shapeL = GenLens[Icon](_.shape)
      val iconL = GenLens[BrandIdentity](_.icon)

      val brandCircleRadiusP: POptional[BrandIdentity, BrandIdentity, Double, Double] =
        iconL andThen shapeL andThen circlePrism

      val brand = BrandIdentity(Logo("red"), Icon("blue", Circle(5.5)))

      // The following will only happen if the "embedded" shape is a Circle - Rectangle etc. will be left intact
      val brandEnlarged = brandCircleRadiusP.modify(_ + 3)(brand)

      brandEnlarged.icon.shape mustBe Circle(8.5)
    }
  }
}