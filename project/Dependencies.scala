import scala.collection.immutable.LinearSeq
import sbt._

object Dependencies {
  def apply(): LinearSeq[ModuleID] =
    LinearSeq(
      scalaReflect, scalaMeta, scalatest, scalatestplus, specs2, munit, scalaMock, scalacheck, scalacheckShapeless, scalatestContainers, s3mock,
      log4Cats, scribe, pprint, pureConfig,
      cats, catsEffect, catsEffectTesting, catsRetry, kittens, catnip, mouse, simulacrum, refined, monocle, shapeless, meowMtl, chimney,
      circe, parserCombinators,
      http4s, monix, fs2, scalaUri, betterFiles, sttp, awsJava, quill, postgresql
    ).flatten

  lazy val scalaReflect: LinearSeq[ModuleID] =
    LinearSeq("org.scala-lang" % "scala-reflect" % BuildProperties("scala.version"))

  lazy val scalaMeta: LinearSeq[ModuleID] =
    LinearSeq("org.scalameta" %% "scalameta" % "4.4.30")

  lazy val scalatest: LinearSeq[ModuleID] =
    LinearSeq("org.scalatest" %% "scalatest" % "3.2.10" % "test, it" withSources() withJavadoc())

  lazy val scalatestplus: LinearSeq[ModuleID] =
    LinearSeq("org.scalatestplus" %% "scalacheck-1-15" % "3.2.10.0" % "test, it" withSources() withJavadoc())

  lazy val specs2: LinearSeq[ModuleID] = {
    val group = "org.specs2"
    val version = "4.13.1"

    List(
      "specs2-core", "specs2-scalacheck", "specs2-matcher-extra", "specs2-cats", "specs2-shapeless"
    ).map(group %% _ % version % "test, it" withSources() withJavadoc())
  }

  lazy val munit: LinearSeq[ModuleID] =
    LinearSeq("org.scalameta" %% "munit" % "0.7.29" % "test, it" withSources() withJavadoc())

  lazy val scalaMock: LinearSeq[ModuleID] =
    LinearSeq("org.scalamock" %% "scalamock" % "5.1.0" % "test, it" withSources() withJavadoc())

  lazy val scalacheck: LinearSeq[ModuleID] =
    LinearSeq("org.scalacheck" %% "scalacheck" % "1.15.4" % "test, it" withSources() withJavadoc())

  lazy val scalacheckShapeless: LinearSeq[ModuleID] =
    LinearSeq("com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.5" withSources() withJavadoc())

  lazy val scalatestContainers: LinearSeq[ModuleID] = {
    val group = "com.dimafeng"
    val version = "1.0.0-alpha1"

    LinearSeq(
      "testcontainers-scala-scalatest", "testcontainers-scala-kafka", "testcontainers-scala-mysql", "testcontainers-scala-postgresql"
    ).map(group %% _ % version % "test, it" withSources() withJavadoc())
  }

  lazy val s3mock: LinearSeq[ModuleID] =
    LinearSeq("io.findify" %% "s3mock" % "0.2.6" % "test, it")

  lazy val log4Cats: LinearSeq[ModuleID] = {
    val group = "org.typelevel"
    val version = "2.1.1"

    LinearSeq(
      "log4cats-core", "log4cats-slf4j"
    ).map(group %% _ % version)
  }

  lazy val scribe: LinearSeq[ModuleID] =
    LinearSeq("com.outr" %% "scribe" % "3.6.3" withSources() withJavadoc())

  lazy val pureConfig: LinearSeq[ModuleID] = {
    val group = "com.github.pureconfig"
    val version = "0.17.1"

    LinearSeq("pureconfig").map(group %% _ % version withSources() withJavadoc())
  }

  lazy val pprint: LinearSeq[ModuleID] =
    LinearSeq("com.lihaoyi" %% "pprint" % "0.6.6")

  lazy val cats: LinearSeq[ModuleID] = {
    val group = "org.typelevel"
    val version = "2.7.0"

    LinearSeq(
      "cats-core", "cats-free"
    ).map(group %% _ % version withSources() withJavadoc()) ++ LinearSeq(
      "cats-laws", "cats-testkit"
    ).map(group %% _ % version % "test, it" withSources() withJavadoc()) ++ LinearSeq(
      "cats-mtl"
    ).map(group %% _ % "1.2.1" withSources() withJavadoc())
  }

  lazy val catsEffect: LinearSeq[ModuleID] = {
    val group = "org.typelevel"
    val version = "3.3.0"

    LinearSeq(
      "cats-effect"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val catsEffectTesting: LinearSeq[ModuleID] =
    LinearSeq("com.codecommit" %% "cats-effect-testing-scalatest" % "0.5.4" % "test, it")

  lazy val catsRetry: LinearSeq[ModuleID] =
    LinearSeq("com.github.cb372" %% "cats-retry" % "3.1.0")

  lazy val kittens: LinearSeq[ModuleID] =
    LinearSeq("org.typelevel" %% "kittens" % "2.3.2")

  lazy val catnip: LinearSeq[ModuleID] =
    LinearSeq("io.scalaland" %% "catnip" % "1.1.2")

  lazy val mouse: LinearSeq[ModuleID] =
    LinearSeq("org.typelevel" %% "mouse" % "1.0.7" withSources() withJavadoc())

  lazy val simulacrum: LinearSeq[ModuleID] =
    LinearSeq("org.typelevel" %% "simulacrum" % "1.0.1" withSources() withJavadoc())

  lazy val refined: LinearSeq[ModuleID] = {
    val group = "eu.timepit"
    val version = "0.9.27"

    LinearSeq(
      "refined", "refined-pureconfig", "refined-cats"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val monocle: LinearSeq[ModuleID] = {
    val group = "com.github.julien-truffaut"
    val version = "2.1.0"

    LinearSeq(
      "monocle-core", "monocle-macro", "monocle-generic"
    ).map(group %% _ % version withSources() withJavadoc()) ++ LinearSeq(
      "monocle-law"
    ).map(group %% _ % version % "test, it" withSources() withJavadoc())
  }

  lazy val shapeless: LinearSeq[ModuleID] =
    LinearSeq("com.chuusai" %% "shapeless" % "2.3.7")

  lazy val meowMtl: LinearSeq[ModuleID] = {
    val group = "com.olegpy"
    val version = "0.5.0"

    LinearSeq(
      "meow-mtl-core", "meow-mtl-effects", "meow-mtl-monix"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val chimney: LinearSeq[ModuleID] =
    LinearSeq("io.scalaland" %% "chimney" % "0.6.1")

  lazy val circe: LinearSeq[ModuleID] = {
    val group = "io.circe"
    val version = "0.14.1"

    LinearSeq(
      "circe-core", "circe-generic", "circe-generic-extras", "circe-parser", "circe-refined"
    ).map(group %% _ % version withSources() withJavadoc()) ++ LinearSeq(
      "circe-testing", "circe-literal"
    ).map(group %% _ % version % "test, it" withSources() withJavadoc())
  }

  lazy val parserCombinators: LinearSeq[ModuleID] =
    LinearSeq("org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0")

  lazy val http4s: LinearSeq[ModuleID] = {
    val group = "org.http4s"
    val version = "1.0.0-M29"

    LinearSeq(
      "http4s-core", "http4s-dsl", "http4s-circe", "http4s-client", "http4s-blaze-client", "http4s-server", "http4s-blaze-server"
    ).map(group %% _ % version withSources() withJavadoc()) ++ LinearSeq(
      "http4s-testing"
    ).map(group %% _ % "1.0.0-M15" % "test, it" withSources() withJavadoc())
  }

  lazy val monix: LinearSeq[ModuleID] = {
    val group = "io.monix"
    val version = "3.4.0"

    LinearSeq(
      "monix", "monix-catnap"
    ).map(group %% _ % version withSources() withJavadoc()) ++ LinearSeq(
      "io.monix" %% "monix-kafka-11" % "1.0.0-RC7" withSources() withJavadoc()
    )
  }

  lazy val fs2: LinearSeq[ModuleID] = {
    val group = "co.fs2"
    val version = "3.2.2"

    LinearSeq(
      "fs2-core", "fs2-io", "fs2-reactive-streams"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val scalaUri: LinearSeq[ModuleID] =
    LinearSeq("io.lemonlabs" %% "scala-uri" % "3.6.0" withSources() withJavadoc())

  lazy val betterFiles: LinearSeq[ModuleID] =
    LinearSeq("com.github.pathikrit" %% "better-files" % "3.9.1" withSources() withJavadoc())

  lazy val sttp: LinearSeq[ModuleID] = {
    val group = "com.softwaremill.sttp.client3"
    val version = "3.3.17"

    LinearSeq(
      "core", "circe", "async-http-client-backend-cats"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val awsJava: LinearSeq[ModuleID] =
    LinearSeq("com.amazonaws" % "aws-java-sdk" % "1.12.120")

  lazy val quill: LinearSeq[ModuleID] = {
    val group = "io.getquill"
    val version = "3.11.0"

    LinearSeq(
      "quill-core", "quill-sql", "quill-jdbc", "quill-codegen-jdbc", "quill-async", "quill-cassandra", "quill-cassandra-monix", "quill-monix", "quill-jdbc-monix", "quill-async-postgres", "quill-codegen"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val postgresql: LinearSeq[ModuleID] =
    LinearSeq("org.postgresql" % "postgresql" % "42.3.1")
}