package tech.backwards.fp.traverse

import cats.implicits._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://www.matfournier.com/2019-10-06-traverse-with-exercises/]]
 *
 * Traverse to sort out nested types:
 * {{{
 *   List[Future[Either[Error, List[Either[Serror, ParsedResponse]]]]]
 * }}}
 * What the heck!
 * In this case it was taking some sort of list of requests, chunking them into batches, making some remote call per batch to some external API that supported batched requests.
 * An entire batch may fail or a batch may succeed but individual elements of the returned json array may have failed for one reason or another.
 *
 * People were contorting themselves into knots over this doing what everyone does when they are new:
 *
 *  - trying to make intermediate variables but from the outside in resulting in many nested map(_.map(_.map(_.map...))) kind of calls
 *  - trying to pattern match their way out of it
 *
 * The real trick is knowing about Travsere and/or sequence.
 */
class TraverseSpec extends AnyWordSpec with Matchers {
  /**
   * {{{
   *  sequence = traverse(identity)
   * }}}
   * If you find yourself mapping a function and then using sequence you can replace that with a single call to traverse.
   */
  "Sequence" should {
    "example" in {
      // We have: List[Option[Int]]
      val xs = List(1.some, 2.some, 3.some)

      // We want: Option[List[Int]] so:
      val result: Option[List[Int]] = xs.sequence

      result mustBe List(1, 2, 3).some
    }
  }
}