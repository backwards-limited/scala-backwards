package com.backwards.aws

import java.io.{File => JFile}
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.language.{implicitConversions, postfixOps}
import better.files._
import cats.data.Kleisli
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
import cats.implicits._
import io.findify.s3mock.S3Mock
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import com.amazonaws.AmazonServiceException
import com.amazonaws.auth.{AWSStaticCredentialsProvider, AnonymousAWSCredentials}
import com.amazonaws.client.builder.AwsClientBuilder.EndpointConfiguration
import com.amazonaws.services.s3.model._
import com.amazonaws.services.s3.{AmazonS3, AmazonS3ClientBuilder}

/**
 * [[https://www.codementor.io/@ysusuk/aws-s3-api-using-scala-du1085b9f Functional Algebra-based API using Scala]]
 *
 * Need:
 * {{{
 *   lazy val s3mock: Seq[ModuleID] = Seq(
 *     "io.findify" %% "s3mock" % "0.2.6" % "test"
 *   )
 * }}}
 */
// TODO - Replace s3mock with Scala Test Containers with S3 docker image
class S3Spec extends AnyWordSpec with Matchers {
  /*type S3Result[A] = AmazonClientException Either A

  implicit def toEither[A](t: Try[A]): S3Result[A] = t match {
    case Success(value) => Right(value)
    case Failure(exception: AmazonClientException) => Left(exception)
  }*/

  /*"Kleisli" should {
    "S3 client" in {
      val s3 = new AmazonS3Client(new ProfileCredentialsProvider())

      val createBucket = Kleisli[S3Result, String, Bucket] { bucketName =>
        Try {
          val bucket = s3.createBucket(new CreateBucketRequest(bucketName))

          s3.setBucketVersioningConfiguration(
            new SetBucketVersioningConfigurationRequest(
              bucketName, new BucketVersioningConfiguration(BucketVersioningConfiguration.ENABLED)
            )
          )

          bucket
        }
      }

      val uploadObject: File => Kleisli[S3Result, String, PutObjectResult] = { file =>
        Kleisli[S3Result, String, PutObjectResult] { bucketName =>
          Try {
            s3.putObject(new PutObjectRequest(bucketName, file.getName, file))
          }
        }
      }

      // If versioning is enabled, delete will put a Delete Marker on the last version of the file
      val deleteObject: String => Kleisli[S3Result, String, Unit] = { fileName =>
        Kleisli[S3Result, String, Unit] { bucketName =>
          Try {
            s3.deleteObject(new DeleteObjectRequest(bucketName, fileName))
          }
        }
      }

      val program: Kleisli[S3Result, String, Unit] = for {
        bucket <- createBucket
        r <- uploadObject(new File("xx"))
        _ <- deleteObject("yy")
      } yield ()

      val blah: S3Result[Unit] = program.run("my-bucket")

      ok
    }
  }*/

  /*"IO" should {
    def createBucket(bucketName: String)(implicit s3: AmazonS3Client): IO[Bucket] =
      IO(s3.createBucket(new CreateBucketRequest(bucketName)))

    def applyVersioning(bucketName: String)(implicit s3: AmazonS3Client): IO[Unit] = IO {
      s3.setBucketVersioningConfiguration(
        new SetBucketVersioningConfigurationRequest(
          bucketName, new BucketVersioningConfiguration(BucketVersioningConfiguration.ENABLED)
        )
      )
    }

    def uploadObject(file: File)(bucketName: String)(implicit s3: AmazonS3Client): IO[PutObjectResult] =
      IO(s3.putObject(new PutObjectRequest(bucketName, file.getName, file)))

    def deleteObject(fileName: String)(bucketName: String)(implicit s3: AmazonS3Client): IO[Unit] =
      IO(s3.deleteObject(new DeleteObjectRequest(bucketName, fileName)))

    "S3 client" in {
      implicit val client: AmazonS3Client = new AmazonS3Client(new ProfileCredentialsProvider())

      val bucket = for {
        bucket <- createBucket("my-bucket")
        _ <- applyVersioning(bucket.getName)
      } yield bucket

      val program = for {
        bucket <- createBucket("blah")
        r <- uploadObject(new File("xx"))(bucket.getName)
        _ <- deleteObject("yy")(bucket.getName)
      } yield ???

      ok
    }

    "S3 client 2" in {
      val program = { implicit s3: AmazonS3Client =>
        for {
          bucket <- createBucket("blah")
          r <- uploadObject(new File("xx"))(bucket.getName)
          _ <- deleteObject("yy")(bucket.getName)
        } yield ???
      }

      IO(new AmazonS3Client(new ProfileCredentialsProvider())).bracket(program)(s3 => IO(s3.shutdown()))

      ok
    }
  }*/

  /*"Kleisli and IO" should {
    "s3 client" in {
      /** Create and start S3 API mock. */
      val api = S3Mock(port = 8001, dir = "/tmp/s3")
      api.start

      def createBucket(implicit s3: AmazonS3): Kleisli[IO, String, Bucket] =
        Kleisli[IO, String, Bucket] { bucketName =>
          IO(s3.createBucket(new CreateBucketRequest(bucketName)))
        }

      def applyVersioning(implicit s3: AmazonS3): Kleisli[IO, String, Unit] =
        Kleisli[IO, String, Unit] { bucketName =>
          IO delay s3.setBucketVersioningConfiguration(
            new SetBucketVersioningConfigurationRequest(
              bucketName, new BucketVersioningConfiguration(BucketVersioningConfiguration.ENABLED)
            )
          )
        }

      val bucket = { implicit s3: AmazonS3 =>
        for {
          bucket <- createBucket
          _ <- applyVersioning
        } yield bucket
      }

      /* AWS S3 client setup.
       *  withPathStyleAccessEnabled(true) trick is required to overcome S3 default
       *  DNS-based bucket access scheme
       *  resulting in attempts to connect to addresses like "bucketname.localhost"
       *  which requires specific DNS setup.
       */
      val endpoint = new EndpointConfiguration("http://localhost:8001", "us-west-2")

      def s3: AmazonS3 = AmazonS3ClientBuilder
        .standard
        .withPathStyleAccessEnabled(true)
        .withEndpointConfiguration(endpoint)
        .withCredentials(new AWSStaticCredentialsProvider(new AnonymousAWSCredentials()))
        .build


      val program = IO(s3).bracket(bucket.andThen(_.run("my-bucket")))(s3 => IO(s3.shutdown()))

      val program2 = IO(s3).bracket(bucket.andThen(_.run("my-bucket")))(s3 => IO(s3.shutdown()))

      println(program.unsafeRunSync())

      api.shutdown

      ok
    }
  }*/

  "Kleisli and IO" should {
    /**
     * AWS S3 client setup.
     *
     * withPathStyleAccessEnabled(true) trick is required to overcome S3 default DNS-based bucket access scheme,
     * resulting in attempts to connect to addresses like "bucketname.localhost", which requires specific DNS setup.
     * @return
     */
    def s3: AmazonS3 =
      AmazonS3ClientBuilder
        .standard
        .withPathStyleAccessEnabled(true)
        .withEndpointConfiguration(new EndpointConfiguration("http://localhost:8001", "us-west-2"))
        .withCredentials(new AWSStaticCredentialsProvider(new AnonymousAWSCredentials()))
        .build

    def createBucket(implicit s3: AmazonS3): Kleisli[IO, String, Bucket] =
      Kleisli[IO, String, Bucket] { bucketName =>
        IO(s3.createBucket(new CreateBucketRequest(bucketName)))
      }

    def applyVersioning(implicit s3: AmazonS3): Kleisli[IO, String, Unit] =
      Kleisli[IO, String, Unit] { bucketName =>
        s3.setBucketVersioningConfiguration(
          new SetBucketVersioningConfigurationRequest(
            bucketName, new BucketVersioningConfiguration(BucketVersioningConfiguration.ENABLED)
          )
        ).pure[IO]
      }

    def uploadObject(file: JFile)(implicit s3: AmazonS3): Kleisli[IO, String, PutObjectResult] =
      Kleisli[IO, String, PutObjectResult] { bucketName =>
        s3.putObject(new PutObjectRequest(bucketName, file.getName, file)).pure[IO]
      }

    def downloadObject(key: String, versionId: String)(implicit s3: AmazonS3): Kleisli[IO, String, S3Object] =
      Kleisli[IO, String, S3Object] { bucketName =>
        s3.getObject(new GetObjectRequest(bucketName, key).withVersionId(versionId)).pure[IO]
      }

    /**
     * Bucket is only deleted if it empty.
     * If versioning is enabled then all file versions will be deleted as well.
     * @param s3
     * @return
     */
    def deleteBucket(implicit s3: AmazonS3): Kleisli[IO, String, Unit] =
      Kleisli[IO, String, Unit] { bucketName =>
        IO {
          s3.listVersions(new ListVersionsRequest().withBucketName(bucketName))
            .getVersionSummaries.iterator().asScala.foreach(v => s3.deleteVersion(bucketName, v.getKey, v.getVersionId))
        } *> IO {
          s3.deleteBucket(bucketName)
        }
      }

    "use s3 client to ." in {
      val bucket: AmazonS3 => Kleisli[IO, String, Bucket] = { implicit s3: AmazonS3 =>
        for {
          bucket <- createBucket
          _ <- applyVersioning
        } yield bucket
      }

      val program: Resource[IO, Bucket] =
        for {
          _ <- Resource.make(IO(S3Mock(port = 8001, dir = "/tmp/s3")).flatTap(api => IO(api.start)))(api => IO(api.shutdown))
          s3 <- Resource.make(IO(s3))(s3 => IO(s3.shutdown()))
          bucket <- Resource.eval(bucket(s3).run("my-bucket"))
        } yield bucket

      println(program.use(IO.pure).unsafeRunSync())
    }

    "use s3 client to .." in {
      val logic: AmazonS3 => Kleisli[IO, String, Bucket] = { implicit s3: AmazonS3 =>
        for {
          bucket <- createBucket
          _ <- applyVersioning
          file <- Kleisli.liftF(IO(File.temporaryFile()))
          // putObjectResult <- uploadObject(file.get.toJava) // TODO - Use ScalaTest with Scala Test Containers to expose S3 Docker
        } yield bucket
      }

      val program: Resource[IO, Bucket] =
        for {
          _ <- Resource.make(IO(S3Mock(port = 8001, dir = "/tmp/s3")).flatTap(api => IO(api.start)))(api => IO(api.shutdown))
          s3 <- Resource.make(IO(s3))(s3 => IO(s3.shutdown()))
          bucket <- Resource.eval(logic(s3).run("my-bucket"))
        } yield bucket

      println(program.use(IO.pure).unsafeRunSync())
    }

    "use s3 client to with exception" in {
      val logic: AmazonS3 => Kleisli[IO, String, Bucket] = { implicit s3: AmazonS3 =>
        for {
          bucket <- createBucket
          _ <- applyVersioning
          _ <- Kleisli.liftF[IO, String, Nothing](IO.raiseError(new AmazonServiceException("Simulating error")))
        } yield bucket
      }

      val program: Resource[IO, Bucket] =
        for {
          _ <- Resource.make(IO(S3Mock(port = 8001, dir = "/tmp/s3")).flatTap(api => IO(api.start)))(api => IO(api.shutdown))
          s3 <- Resource.make(IO(s3))(s3 => IO(s3.shutdown()))
          bucket <- Resource.eval(logic(s3).run("my-bucket"))
        } yield bucket

      println(program.use(IO.pure).attempt.unsafeRunSync())
    }
  }
}