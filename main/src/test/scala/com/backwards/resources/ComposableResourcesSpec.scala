package com.backwards.resources

import scala.io.{BufferedSource, Source}
import cats.Monad
import cats.effect.{IO, Resource}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import cats.implicits._

/**
 * [[https://bszwej.medium.com/composable-resource-management-in-scala-ce902bda48b2 Composable resource management]]
 */
class ComposableResourcesSpec extends AnyWordSpec with Matchers {
  "Without an effect type - Resource management with Loaner pattern" should {
    "work for one resource" in {
      def withSource[T](source: Source)(handle: Source => T): T =
        try {
          pprint.pprintln(s"Before source")
          val result = handle(source)
          pprint.pprintln("After source")
          result
        } finally {
          pprint.pprintln(s"Closing source")
          source.close()
        }

      withSource(Source.fromResource("application.dev1.conf")) { source: Source =>
        pprint.pprintln("Using source")
        val result = source.getLines.mkString(start = "", sep = "\n", end = "")
        pprint.pprintln(result)
        result
      }

      pprint.pprintln("Done")
    }

    "work for two composed generic resources" in {
      def withResource[R, T](resource: R)(close: R => Unit)(handle: R => T): T =
        try {
          pprint.pprintln(s"Before resource: ${resource.hashCode}")
          val result = handle(resource)
          pprint.pprintln(s"After resource: ${resource.hashCode}")
          result
        } finally {
          pprint.pprintln(s"Closing resource: ${resource.hashCode}")
          close(resource)
        }

      withResource(Source.fromResource("application.dev1.conf"))(_.close()) { source1 =>
        withResource(Source.fromResource("application.dev2.conf"))(_.close()) { source2 =>
          pprint.pprintln(s"Using source2: ${source2.hashCode}")
          val result = source2.getLines.mkString(start = "", sep = "\n", end = "")
          pprint.pprintln(result)
          result
        }
      }

      pprint.pprintln("Done")
    }

    "improve composition of generic resources" in {
      trait Resource[R] {
        def use[U](f: R => U): U
      }

      object Resource {
        def make[R](acquire: => R)(close: R => Unit): Resource[R] = new Resource[R] {
          override def use[U](f: R => U): U = {
            val resource = acquire

            try {
              pprint.pprintln(s"Before resource: ${resource.hashCode}")
              val results = f(resource)
              pprint.pprintln(s"After resource: ${resource.hashCode}")
              results
            } finally {
              pprint.pprintln(s"Closing resource: ${resource.hashCode}")
              close(resource)
            }
          }
        }
      }

      Resource.make(Source.fromResource("application.dev1.conf"))(_.close()).use { source1 =>
        Resource.make(Source.fromResource("application.dev2.conf"))(_.close()).use { source2 =>
          pprint.pprintln(s"Using source2: ${source2.hashCode}")
          val result = source2.getLines.mkString(start = "", sep = "\n", end = "")
          pprint.pprintln(result)
          result
        }
      }
    }

    "improve the composition again with Monad (because the last improvement wasn't much of an 'upgrade')" in {
      // i.e. we'd like to compose our new data type sequentially, and sequential composition is the essence of..... a Monad!

      trait Resource[R] {
        def use[U](f: R => U): U
      }

      object Resource {
        def make[R](acquire: => R)(close: R => Unit): Resource[R] = new Resource[R] {
          override def use[U](f: R => U): U = {
            val resource = acquire

            try {
              pprint.pprintln(s"Before resource: ${resource.hashCode}")
              val results = f(resource)
              pprint.pprintln(s"After resource: ${resource.hashCode}")
              results
            } finally {
              pprint.pprintln(s"Closing resource: ${resource.hashCode}")
              close(resource)
            }
          }
        }
      }

      implicit val resourceMonad: Monad[Resource] =
        new Monad[Resource] {
          def pure[R](r: R): Resource[R] =
            Resource.make(r)(_ => ())

          def flatMap[A, B](fa: Resource[A])(fab: A => Resource[B]): Resource[B] =
            new Resource[B] {
              def use[U](fbu: B => U): U =
                fa.use(a => fab(a).use(b => fbu(b)))
            }

          def tailRecM[A, B](a: A)(fab: A => Resource[A Either B]): Resource[B] = ???
        }

      val resource1: Resource[BufferedSource] =
        Resource.make(Source.fromResource("application.dev1.conf"))(_.close())

      val resource2: Resource[BufferedSource] =
        Resource.make(Source.fromResource("application.dev2.conf"))(_.close())

      val resources: Resource[(BufferedSource, BufferedSource)] = resource1.flatMap(source1 =>
        resource2.map(source2 =>
          source1 -> source2
        )
      )

      resources.use { case (source1, source2) =>
        pprint.pprintln(s"Using source2: ${source2.hashCode}")
        val result = source2.getLines.mkString(start = "", sep = "\n", end = "")
        pprint.pprintln(result)
        result
      }

      pprint.pprintln("Done")
    }
  }

  "Cats Effect" should {
    class AwsSdkJavaClient {
      println("Opening connections")

      def execute(): Unit = println("Executing")

      def close(): Unit = println("Closing connections")
    }

    def businessLogic(client: AwsSdkJavaClient): IO[Unit] =
      IO(client.execute())

    "a tad naive" in {
      def program: IO[Unit] =
        IO(new AwsSdkJavaClient).flatTap(businessLogic).flatMap(client => IO(client.close()))

      program.unsafeRunSync
    }

    "improve with Bracket" in {
      def program: IO[Unit] =
        IO(new AwsSdkJavaClient).bracket(businessLogic)(client => IO(client.close()))

      program.unsafeRunSync
    }

    "but Bracket struggles to scale for multiple resources" in {
      def businessLogic(client: AwsSdkJavaClient, config: Source): IO[Unit] =
        IO(client.execute())

      def program: IO[Unit] =
        IO(new AwsSdkJavaClient).bracket(awsClient =>
          IO(Source.fromResource("application.dev1.conf")).bracket(config =>
            businessLogic(awsClient, config)
          )(config => IO(config.close))
        )(awsClient => IO(awsClient.close()))

      program.unsafeRunSync
    }
  }

  "Cats Resource" should {
    class AwsSdkJavaClient {
      println("Opening connections")

      def execute(): Unit = println("Executing")

      def close(): Unit = println("Closing connections")
    }

    "demo" in {
      def businessLogic(awsClient: AwsSdkJavaClient, config: Source): IO[Unit] =
        IO(awsClient.execute())

      def program: Resource[IO, Unit] =
        for {
          awsClient <- Resource.make(IO(new AwsSdkJavaClient))(r => IO(r.close()))
          config    <- Resource.make(IO(Source.fromResource("application.dev1.conf")))(r => IO(r.close()))
          _      <- Resource.liftF(businessLogic(awsClient, config))
        } yield ()

      program.use(_ => IO.unit).unsafeRunSync
    }

    "and then there's applicative compose" in {
      def businessLogic(awsClient: AwsSdkJavaClient, config: Source): IO[Unit] =
        IO(awsClient.execute())

      val awsClientResourse: Resource[IO, AwsSdkJavaClient] =
        Resource.make(IO(new AwsSdkJavaClient))(r => IO(r.close()))

      val configResource: Resource[IO, BufferedSource] =
        Resource.make(IO(Source.fromResource("application.dev1.conf")))(r => IO(r.close()))

      (awsClientResourse, configResource).tupled.use { case (awsClient, config) =>
        businessLogic(awsClient, config)
      } unsafeRunSync
    }
  }
}