import sbt._

object Dependencies {
  def apply(): Seq[ModuleID] =
    List(
      scalaMeta, scalatest, scalatestplus, specs2, munit, munitCatsEffect, weaver, scalaMock, scalacheck, scalacheckShapeless, scalatestContainers, s3mock, magnolify,
      log4Cats, scribe, pprint, pureConfig,
      cats, catsEffect, catsEffectTesting, catsRetry, kittens, mouse, simulacrum, refined, monocle, shapeless, /*meowMtl,*/ chimney,
      scalaParallelCollections, parserCombinators, http4s, fs2, scalaUri, betterFiles, sttp, caliban, awsJava, quill, postgresql,
      circe, playJson
    ).flatten

  lazy val scalaMeta: Seq[ModuleID] =
    List("org.scalameta" %% "scalameta" % "4.7.1")

  lazy val scalatest: Seq[ModuleID] =
    List("org.scalatest" %% "scalatest" % "3.2.14" % "test, it" withSources() withJavadoc())

  lazy val scalatestplus: Seq[ModuleID] =
    List("org.scalatestplus" %% "scalacheck-1-15" % "3.2.11.0" % "test, it" withSources() withJavadoc())

  lazy val specs2: Seq[ModuleID] = {
    val group = "org.specs2"
    val version = "4.19.0"

    List(
      "specs2-core", "specs2-scalacheck", "specs2-matcher-extra", "specs2-cats", "specs2-shapeless"
    ).map(group %% _ % version % "test, it" withSources() withJavadoc())
  }

  lazy val munit: Seq[ModuleID] = {
    val group = "org.scalameta"
    val version = "1.0.0-M7"

    List(
      "munit", "munit-scalacheck"
    ).map(group %% _ % version % "test, it" withSources() withJavadoc() exclude("org.typelevel", "munit-cats-effect-2_2.13") exclude("org.typelevel", "munit-cats-effect-2")) ++ List(
      "org.typelevel" %% "discipline-munit" % "2.0.0-M3" % "test, it"
    )
  }

  lazy val munitCatsEffect: Seq[ModuleID] =
    List("org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % "test, it" force() withSources() withJavadoc())

  lazy val weaver: Seq[ModuleID] =
    List("com.disneystreaming" %% "weaver-cats" % "0.8.1" % "test, it" withSources() withJavadoc())

  lazy val scalaMock: Seq[ModuleID] =
    List("org.scalamock" %% "scalamock" % "5.2.0" % "test, it" withSources() withJavadoc())

  lazy val scalacheck: Seq[ModuleID] =
    List("org.scalacheck" %% "scalacheck" % "1.17.0" % "test, it" withSources() withJavadoc())

  lazy val scalacheckShapeless: Seq[ModuleID] =
    List("com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.5" withSources() withJavadoc())

  lazy val scalatestContainers: Seq[ModuleID] = {
    val group = "com.dimafeng"
    val version = "1.0.0-alpha1"

    List(
      "testcontainers-scala-scalatest", "testcontainers-scala-kafka", "testcontainers-scala-mysql", "testcontainers-scala-postgresql"
    ).map(group %% _ % version % "test, it" withSources() withJavadoc())
  }

  lazy val s3mock: Seq[ModuleID] =
    List("io.findify" %% "s3mock" % "0.2.6" % "test, it")

  lazy val magnolify: Seq[ModuleID] =
    List("com.spotify" %% "magnolify-scalacheck" % "0.6.2" % "test, it")

  lazy val log4Cats: Seq[ModuleID] = {
    val group = "org.typelevel"
    val version = "2.5.0"

    List(
      "log4cats-core", "log4cats-slf4j"
    ).map(group %% _ % version)
  }

  lazy val scribe: Seq[ModuleID] =
    List("com.outr" %% "scribe" % "3.10.5" withSources() withJavadoc())

  lazy val pureConfig: Seq[ModuleID] = {
    val group = "com.github.pureconfig"
    val version = "0.17.2"

    List("pureconfig").map(group %% _ % version withSources() withJavadoc())
  }

  lazy val pprint: Seq[ModuleID] =
    List("com.lihaoyi" %% "pprint" % "0.8.1")

  lazy val cats: Seq[ModuleID] = {
    val group = "org.typelevel"
    val version = "2.9.0"

    List(
      "cats-core", "cats-free"
    ).map(group %% _ % version withSources() withJavadoc()) ++ List(
      "cats-laws", "cats-testkit"
    ).map(group %% _ % version % "test, it" withSources() withJavadoc()) ++ List(
      "cats-mtl"
    ).map(group %% _ % "1.3.0" withSources() withJavadoc())
  }

  lazy val catsEffect: Seq[ModuleID] = {
    val group = "org.typelevel"
    val version = "3.4.4"

    List(
      "cats-effect"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val catsEffectTesting: Seq[ModuleID] = {
    val group = "org.typelevel"
    val version = "1.5.0"

    List(
      "cats-effect-testing-scalatest"
    ).map(group %% _ % version % "test, it" withSources() withJavadoc())
  }

  lazy val catsRetry: Seq[ModuleID] =
    List("com.github.cb372" %% "cats-retry" % "3.1.0")

  lazy val kittens: Seq[ModuleID] =
    List("org.typelevel" %% "kittens" % "3.0.0")

  lazy val mouse: Seq[ModuleID] =
    List("org.typelevel" %% "mouse" % "1.2.1" withSources() withJavadoc())

  lazy val simulacrum: Seq[ModuleID] =
    List("org.typelevel" %% "simulacrum" % "1.0.1" withSources() withJavadoc())

  lazy val refined: Seq[ModuleID] = {
    val group = "eu.timepit"
    val version = "0.10.1"

    List(
      "refined", "refined-pureconfig", "refined-cats"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val monocle: Seq[ModuleID] = {
    val group = "dev.optics"
    val version = "3.2.0"

    List(
      "monocle-core", "monocle-macro", "monocle-generic"
    ).map(group %% _ % version withSources() withJavadoc()) ++ List(
      "monocle-law"
    ).map(group %% _ % version % "test, it" withSources() withJavadoc())
  }

  lazy val shapeless: Seq[ModuleID] =
    List("com.chuusai" %% "shapeless" % "2.3.10")

  lazy val meowMtl: Seq[ModuleID] = {
    val group = "com.olegpy"
    val version = "0.5.0"

    List(
      "meow-mtl-core", "meow-mtl-effects", "meow-mtl-monix"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val chimney: Seq[ModuleID] =
    List("io.scalaland" %% "chimney" % "0.6.2")

  lazy val scalaParallelCollections: Seq[ModuleID] =
    List("org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4")

  lazy val parserCombinators: Seq[ModuleID] =
    List("org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1")

  lazy val http4s: Seq[ModuleID] = {
    val group = "org.http4s"
    val version = "1.0.0-M36"

    List(
      "http4s-core", "http4s-dsl", "http4s-circe", "http4s-client", "http4s-blaze-client", "http4s-server", "http4s-blaze-server"
    ).map(group %% _ % version withSources() withJavadoc()) ++ List(
      "http4s-testing"
    ).map(group %% _ % "1.0.0-M15" % "test, it" withSources() withJavadoc())
  }

  lazy val fs2: Seq[ModuleID] = {
    val group = "co.fs2"
    val version = "3.4.0"

    List(
      "fs2-core", "fs2-io", "fs2-reactive-streams"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val scalaUri: Seq[ModuleID] =
    List("io.lemonlabs" %% "scala-uri" % "4.0.3" withSources() withJavadoc())

  lazy val betterFiles: Seq[ModuleID] =
    List("com.github.pathikrit" %% "better-files" % "3.9.1" withSources() withJavadoc())

  lazy val sttp: Seq[ModuleID] = {
    val group = "com.softwaremill.sttp.client3"
    val version = "3.8.6"

    List(
      "core", "circe", "async-http-client-backend-cats", "async-http-client-backend-zio"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val caliban: Seq[ModuleID] = {
    val group = "com.github.ghostdogpr"
    val version = "2.0.2"

    List(
      "caliban-client"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val awsJava: Seq[ModuleID] =
    List("com.amazonaws" % "aws-java-sdk" % "1.12.377")

  lazy val quill: Seq[ModuleID] = {
    val group = "io.getquill"

    List(
      "quill-async", "quill-async-postgres"
    ).map(group %% _ % "3.12.0" withSources() withJavadoc()) ++
    List(
      "quill-core", "quill-sql", "quill-jdbc", "quill-codegen-jdbc", "quill-codegen"
    ).map(group %% _ % "4.6.0" withSources() withJavadoc())
  }

  lazy val postgresql: Seq[ModuleID] =
    List("org.postgresql" % "postgresql" % "42.5.1")

  lazy val circe: Seq[ModuleID] = {
    val group = "io.circe"
    val version = "0.14.3"

    List(
      "circe-core", "circe-generic", "circe-generic-extras", "circe-parser", "circe-refined"
    ).map(group %% _ % version withSources() withJavadoc()) ++ List(
      "circe-testing", "circe-literal"
    ).map(group %% _ % version % "test, it" withSources() withJavadoc())
  }

  lazy val playJson: Seq[ModuleID] =
    List("com.typesafe.play" %% "play-json" % "2.10.0-RC7")
}