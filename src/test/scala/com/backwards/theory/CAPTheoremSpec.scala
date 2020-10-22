package com.backwards.theory

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://medium.com/better-programming/what-is-the-cap-theorem-and-where-is-it-used-363475aa8db6]]
 *
 * It is impossible for any distributed database system to provide more than two of the following properties together:
 *
 *  - Consistency
 *  - Availability
 *  - Partition Tolerance
 *
 * A consistent system is one in which all nodes see the same data at the same time.
 *
 * A highly available distributed system is one that remains operational 100% of the time.
 *
 * It states that a system should continue to run even if the connection between nodes delays or breaks. Note: This doesn’t mean nodes have gone down. Nodes are up but can’t communicate.
 */
class CAPTheoremSpec extends AnyWordSpec with Matchers {
  "" should {
    "" in {

    }
  }
}