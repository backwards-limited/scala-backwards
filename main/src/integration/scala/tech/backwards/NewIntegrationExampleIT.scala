package tech.backwards

import cats.effect.std.Dispatcher
import cats.effect.{IO, SyncIO}
import munit.CatsEffectSuite

/**
 * https://yadukrishnan.live/unlocking-the-power-of-sbt-a-beginners-guide-to-understanding-not-so-common-features#heading-6-integration-tests
 *
 * sbt Integration/test
 *
 * https://www.jan0sch.de/post/scala-sbt-switch-from-integrationtest-config-to-tagged-tests/
 *
 * testOnly -- --include-tags=IntegrationTest
 *
 * testOnly -- --exclude-tags=IntegrationTest
 */
class NewIntegrationExampleIT extends CatsEffectSuite with Tags {
  test("tests can return IO[Unit] with assertions expressed via a map".tag(IntegrationTest))(
    IO(42).map(it => assertEquals(it, 42))
  )

  test("alternatively, assertions can be written via assertIO".tag(IntegrationTest))(
    assertIO(IO(42), 42)
  )

  test("or via assertEquals syntax".tag(IntegrationTest))(
    IO(42).assertEquals(42)
  )

  test("or via plain assert syntax on IO[Boolean]".tag(IntegrationTest))(
    IO(true).assert
  )

  test("SyncIO works too".tag(IntegrationTest))(
    SyncIO(42).assertEquals(42)
  )

  val dispatcher: SyncIO[FunFixture[Dispatcher[IO]]] =
    ResourceFunFixture(Dispatcher.parallel[IO])

  dispatcher.test("resources can be lifted to munit fixtures".tag(IntegrationTest))(dsp =>
    dsp.unsafeRunAndForget(IO(42))
  )
}
