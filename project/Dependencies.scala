import sbt._

object Dependencies {
  def apply(): Seq[ModuleID] = Seq(
    scalaReflect, scalaMeta, scalatest, scalatestplus, specs2, munit, scalaMock, scalacheck, scalacheckShapeless, /*testcontainers,*/ scalaTestContainers, s3mock,
    log4Cats, scribe, pprint, pureConfig,
    cats, catsEffect, catsEffectTesting, catsRetry, kittens, catnip, mouse, simulacrum, refined, monocle, shapeless, meowMtl, chimney,
    circe, parserCombinators,
    http4s, monix, fs2, scalaUri, betterFiles, sttp, awsJava, quill, postgresql
  ).flatten

  lazy val scalaReflect: Seq[ModuleID] = Seq(
    "org.scala-lang" % "scala-reflect" % BuildProperties("scala.version")
  )

  lazy val scalaMeta: Seq[ModuleID] = Seq(
    "org.scalameta" %% "scalameta" % "4.4.20"
  )
  
  lazy val scalatest: Seq[ModuleID] = Seq(
    "org.scalatest" %% "scalatest" % "3.2.9" % "test, it" withSources() withJavadoc()
  )

  lazy val scalatestplus: Seq[ModuleID] = Seq(
    "org.scalatestplus" %% "scalacheck-1-15" % "3.2.9.0" % "test, it" withSources() withJavadoc()
  )

  lazy val specs2: Seq[ModuleID] = {
    val group = "org.specs2"
    val version = "4.12.0"

    Seq(
      "specs2-core", "specs2-scalacheck", "specs2-matcher-extra", "specs2-cats", "specs2-shapeless"
    ).map(group %% _ % version % "test, it" withSources() withJavadoc())
  }

  lazy val munit: Seq[ModuleID] = Seq(
    "org.scalameta" %% "munit" % "0.7.27" % "test, it" withSources() withJavadoc()
  )

  lazy val scalaMock: Seq[ModuleID] = Seq(
    "org.scalamock" %% "scalamock" % "5.1.0" % "test, it" withSources() withJavadoc()
  )

  lazy val scalacheck: Seq[ModuleID] = Seq(
    "org.scalacheck" %% "scalacheck" % "1.15.4" % "test, it" withSources() withJavadoc()
  )

  lazy val scalacheckShapeless: Seq[ModuleID] = Seq(
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.5" withSources() withJavadoc()
  )
  
  /*lazy val testcontainers: Seq[ModuleID] = Seq(
    "org.testcontainers" % "testcontainers" % "1.15.2" % "test, it" withSources() withJavadoc()
  )*/

  lazy val scalaTestContainers: Seq[ModuleID] = {
    val group = "com.dimafeng"
    val version = "1.0.0-alpha1"

    Seq(
      "testcontainers-scala-scalatest", "testcontainers-scala-kafka", "testcontainers-scala-mysql", "testcontainers-scala-postgresql"
    ).map(group %% _ % version % "test, it" withSources() withJavadoc())
  }

  lazy val s3mock: Seq[ModuleID] = Seq(
    "io.findify" %% "s3mock" % "0.2.6" % "test, it"
  )

  lazy val log4Cats: Seq[ModuleID] = {
    val group = "org.typelevel"
    val version = "2.1.1"

    Seq(
      "log4cats-core", "log4cats-slf4j"
    ).map(group %% _ % version)
  }

  lazy val scribe: Seq[ModuleID] = Seq(
    "com.outr" %% "scribe" % "3.5.5" withSources() withJavadoc()
  )
  
  lazy val pureConfig: Seq[ModuleID] = {
    val group = "com.github.pureconfig"
    val version = "0.15.0"

    Seq("pureconfig").map(group %% _ % version withSources() withJavadoc())
  }

  lazy val pprint: Seq[ModuleID] = Seq(
    "com.lihaoyi" %% "pprint" % "0.6.6"
  )

  lazy val cats: Seq[ModuleID] = {
    val group = "org.typelevel"
    val version = "2.6.1"

    Seq(
      "cats-core", "cats-effect", "cats-free"
    ).map(group %% _ % version withSources() withJavadoc()) ++ Seq(
      "cats-laws", "cats-testkit"
    ).map(group %% _ % version % "test, it" withSources() withJavadoc())
  }

  lazy val catsEffect: Seq[ModuleID] = {
    val group = "org.typelevel"
    val version = "3.1.1"

    Seq(
      "cats-effect"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val catsEffectTesting: Seq[ModuleID] = Seq(
    "com.codecommit" %% "cats-effect-testing-scalatest" % "0.5.4" % "test, it"
  )

  lazy val catsRetry: Seq[ModuleID] = Seq(
    "com.github.cb372" %% "cats-retry" % "3.0.0"
  )

  lazy val kittens: Seq[ModuleID] = Seq(
    "org.typelevel" %% "kittens" % "2.3.2"
  )

  lazy val catnip: Seq[ModuleID] = Seq(
    "io.scalaland" %% "catnip" % "1.1.2"
  )

  lazy val mouse: Seq[ModuleID] = Seq(
    "org.typelevel" %% "mouse" % "1.0.3" withSources() withJavadoc()
  )

  lazy val simulacrum: Seq[ModuleID] = Seq(
    "org.typelevel" %% "simulacrum" % "1.0.1" withSources() withJavadoc()
  )
  
  lazy val refined: Seq[ModuleID] = {
    val group = "eu.timepit"
    val version = "0.9.26"

    Seq(
      "refined", "refined-pureconfig", "refined-cats"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val monocle: Seq[ModuleID] = {
    val group = "com.github.julien-truffaut"
    val version = "2.1.0"

    Seq(
      "monocle-core", "monocle-macro", "monocle-generic"
    ).map(group %% _ % version withSources() withJavadoc()) ++ Seq(
      "monocle-law"
    ).map(group %% _ % version % "test, it" withSources() withJavadoc())
  }

  lazy val shapeless: Seq[ModuleID] = Seq(
    "com.chuusai" %% "shapeless" % "2.3.7"
  )

  lazy val meowMtl: Seq[ModuleID] = {
    val group = "com.olegpy"
    val version = "0.4.1"

    Seq(
      "meow-mtl-core", "meow-mtl-effects", "meow-mtl-monix"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val chimney: Seq[ModuleID] = Seq(
    "io.scalaland" %% "chimney" % "0.6.1"
  )

  lazy val circe: Seq[ModuleID] = {
    val group = "io.circe"
    val version = "0.14.1"

    Seq(
      "circe-core", "circe-generic", "circe-generic-extras", "circe-parser", "circe-refined"
    ).map(group %% _ % version withSources() withJavadoc()) ++ Seq(
      "circe-testing", "circe-literal"
    ).map(group %% _ % version % "test, it" withSources() withJavadoc())
  }

  lazy val parserCombinators: Seq[ModuleID] = Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "2.0.0"
  )

  lazy val http4s: Seq[ModuleID] = {
    val group = "org.http4s"
    val version = "1.0.0-M23"

    Seq(
      "http4s-core", "http4s-dsl", "http4s-circe", "http4s-client", "http4s-blaze-client", "http4s-server", "http4s-blaze-server"
    ).map(group %% _ % version withSources() withJavadoc()) ++ Seq(
      "http4s-testing"
    ).map(group %% _ % "1.0.0-M15" % "test, it" withSources() withJavadoc())
  }

  lazy val monix: Seq[ModuleID] = {
    val group = "io.monix"
    val version = "3.4.0"

    Seq(
      "monix", "monix-catnap"
    ).map(group %% _ % version withSources() withJavadoc()) ++ Seq(
      "io.monix" %% "monix-kafka-11" % "1.0.0-RC7" withSources() withJavadoc()
    )
  }

  lazy val fs2: Seq[ModuleID] = {
    val group = "co.fs2"
    val version = "3.0.4"

    Seq(
      "fs2-core", "fs2-io", "fs2-reactive-streams"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val scalaUri: Seq[ModuleID] = Seq(
    "io.lemonlabs" %% "scala-uri" % "3.2.0" withSources() withJavadoc()
  )
  
  lazy val betterFiles: Seq[ModuleID] = Seq(
    "com.github.pathikrit" %% "better-files" % "3.9.1" withSources() withJavadoc()
  )

  lazy val sttp: Seq[ModuleID] = {
    val group = "com.softwaremill.sttp.client3"
    val version = "3.3.5"

    Seq(
      "core", "circe", "async-http-client-backend-cats"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val awsJava: Seq[ModuleID] = Seq(
    "com.amazonaws" % "aws-java-sdk" % "1.11.1030"
  )

  lazy val quill: Seq[ModuleID] = {
    val group = "io.getquill"
    val version = "3.7.1"

    Seq(
      "quill-core", "quill-sql", "quill-jdbc", "quill-codegen-jdbc", "quill-async", "quill-cassandra", "quill-cassandra-monix", "quill-monix", "quill-jdbc-monix", "quill-async-postgres", "quill-codegen"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val postgresql: Seq[ModuleID] = Seq(
    "org.postgresql" % "postgresql" % "42.2.20"
  )
}