package com.backwards.fp.monad

import org.scalatest.MustMatchers
import org.scalatest.wordspec.AnyWordSpec
import com.backwards.fp.Id

class IdSpec extends AnyWordSpec with MustMatchers {
  import com.backwards.fp.monad.IdOps._
  import com.backwards.fp.monad.MonadOps._

  "Id Monad" should {
    "work" in {
      { 10.pure[Id] >>= { x: Int => Id(x + 10)} } mustBe 20.pure[Id]
    }
  }
}