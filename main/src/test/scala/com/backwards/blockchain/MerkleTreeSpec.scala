package com.backwards.blockchain

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * [[https://www.works-hub.com/learn/grokking-merkle-tree-bccc4 Grokking Merkle Tree]]
 */
class MerkleTreeSpec extends AnyWordSpec with Matchers {
  "" should {
    "" in {

    }
  }

  trait Node[T] {
    val hash: T
  }

  /**
   * A node that does not have any children nodes and stores the hashed value of the input data.
   * @param datum
   * @tparam T
   */
  final case class Leaf[T](datum: T) extends Node[T] {
    override val hash: T = ???
  }

  /**
   * A node that is produced as a product of the hash value of its children nodes.
   * @param left
   * @param right
   * @tparam T
   */
  final case class Branch[T](left: Node[T], right: Option[Node[T]]) extends Node[T] {
    override val hash: T = ???
  }
}