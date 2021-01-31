import sbt._

object Dependencies {
  def apply(): Seq[ModuleID] = Seq(
    scalaReflect, scalaMeta, scalatest, scalaMock, scalacheck, scalacheckShapeless, testcontainers, scalaTestContainers, s3mock,
    log4Cats, scribe, pprint, pureConfig,
    cats, catsEffectTesting, catsRetry, kittens, catnip, mouse, simulacrum, refined, monocle, shapeless, meowMtl,
    http4s, monix, fs2, scalaUri, betterFiles, sttp, awsJava,
    circe, parserCombinators
  ).flatten

  lazy val scalaReflect: Seq[ModuleID] = Seq(
    "org.scala-lang" % "scala-reflect" % BuildProperties("scala.version")
  )

  lazy val scalaMeta: Seq[ModuleID] = Seq(
    "org.scalameta" %% "scalameta" % "4.4.7"
  )
  
  lazy val scalatest: Seq[ModuleID] = Seq(
    "org.scalatest" %% "scalatest" % "3.2.3" % "test, it" withSources() withJavadoc()
  )

  lazy val scalaMock: Seq[ModuleID] = Seq(
    "org.scalamock" %% "scalamock" % "5.1.0" % "test, it" withSources() withJavadoc()
  )

  lazy val scalacheck: Seq[ModuleID] = Seq(
    "org.scalacheck" %% "scalacheck" % "1.15.2" % "test, it" withSources() withJavadoc()
  )

  lazy val scalacheckShapeless: Seq[ModuleID] = Seq(
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.5" withSources() withJavadoc()
  )
  
  lazy val testcontainers: Seq[ModuleID] = Seq(
    "org.testcontainers" % "testcontainers" % "1.15.1" % "test, it" withSources() withJavadoc()
  )

  lazy val scalaTestContainers: Seq[ModuleID] = {
    val group = "com.dimafeng"
    val version = "1.0.0-alpha1"

    Seq(
      "testcontainers-scala-scalatest", "testcontainers-scala-kafka", "testcontainers-scala-mysql"
    ).map(group %% _ % version % "test, it" withSources() withJavadoc())
  }

  lazy val s3mock: Seq[ModuleID] = Seq(
    "io.findify" %% "s3mock" % "0.2.6" % "test, it"
  )

  lazy val log4Cats: Seq[ModuleID] = {
    val group = "io.chrisdavenport"
    val version = "1.1.1"

    Seq(
      "log4cats-core", "log4cats-slf4j"
    ).map(group %% _ % version)
  }

  lazy val scribe: Seq[ModuleID] = Seq(
    "com.outr" %% "scribe" % "3.2.4" withSources() withJavadoc()
  )
  
  lazy val pureConfig: Seq[ModuleID] = {
    val group = "com.github.pureconfig"
    val version = "0.14.0"

    Seq("pureconfig").map(group %% _ % version withSources() withJavadoc())
  }

  lazy val pprint: Seq[ModuleID] = Seq(
    "com.lihaoyi" %% "pprint" % "0.6.0"
  )

  lazy val cats: Seq[ModuleID] = {
    val group = "org.typelevel"
    val version = "2.3.1"

    Seq(
      "cats-core", "cats-effect", "cats-free"
    ).map(group %% _ % version withSources() withJavadoc()) ++ Seq(
      "cats-laws", "cats-testkit"
    ).map(group %% _ % version % "test, it" withSources() withJavadoc())
  }

  lazy val catsEffectTesting: Seq[ModuleID] = Seq(
    "com.codecommit" %% "cats-effect-testing-scalatest" % "0.5.0" % "test, it"
  )

  lazy val catsRetry: Seq[ModuleID] = Seq(
    "com.github.cb372" %% "cats-retry" % "2.1.0"
  )

  lazy val kittens: Seq[ModuleID] = Seq(
    "org.typelevel" %% "kittens" % "2.2.1"
  )

  lazy val catnip: Seq[ModuleID] = Seq(
    "io.scalaland" %% "catnip" % "1.1.2"
  )

  lazy val mouse: Seq[ModuleID] = Seq(
    "org.typelevel" %% "mouse" % "0.26.2" withSources() withJavadoc()
  )

  lazy val simulacrum: Seq[ModuleID] = Seq(
    "org.typelevel" %% "simulacrum" % "1.0.1" withSources() withJavadoc()
  )
  
  lazy val refined: Seq[ModuleID] = {
    val group = "eu.timepit"
    val version = "0.9.20"

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
    "com.chuusai" %% "shapeless" % "2.3.3"
  )

  lazy val meowMtl: Seq[ModuleID] = {
    val group = "com.olegpy"
    val version = "0.4.1"

    Seq(
      "meow-mtl-core", "meow-mtl-effects", "meow-mtl-monix"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val http4s: Seq[ModuleID] = {
    val group = "org.http4s"
    val version = "1.0-234-d1a2b53"

    Seq(
      "http4s-core", "http4s-dsl", "http4s-circe", "http4s-client", "http4s-blaze-client", "http4s-server", "http4s-blaze-server"
    ).map(group %% _ % version withSources() withJavadoc()) ++ Seq(
      "http4s-testing"
    ).map(group %% _ % version % "test, it" withSources() withJavadoc())
  }

  lazy val monix: Seq[ModuleID] = {
    Seq(
      "io.monix" %% "monix" % "3.3.0" withSources() withJavadoc()
    ) ++ Seq(
      "io.monix" %% "monix-kafka-11" % "1.0.0-RC7" withSources() withJavadoc()
    )
  }

  lazy val fs2: Seq[ModuleID] = {
    val group = "co.fs2"
    val version = "2.5.0"

    Seq(
      "fs2-core", "fs2-io", "fs2-reactive-streams"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val scalaUri: Seq[ModuleID] = Seq(
    "io.lemonlabs" %% "scala-uri" % "2.3.1" withSources() withJavadoc()
  )
  
  lazy val betterFiles: Seq[ModuleID] = Seq(
    "com.github.pathikrit" %% "better-files" % "3.9.1" withSources() withJavadoc()
  )

  lazy val sttp: Seq[ModuleID] = {
    val group = "com.softwaremill.sttp.client"
    val version = "2.2.9"

    Seq(
      "core", "circe", "async-http-client-backend-cats"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val awsJava: Seq[ModuleID] = Seq(
    "com.amazonaws" % "aws-java-sdk" % "1.11.943"
  )

  lazy val circe: Seq[ModuleID] = {
    val group = "io.circe"
    val version = "0.13.0"

    Seq(
      "circe-core", "circe-generic", "circe-generic-extras", "circe-parser", "circe-refined"
    ).map(group %% _ % version withSources() withJavadoc()) ++ Seq(
      "circe-testing", "circe-literal"
    ).map(group %% _ % version % "test, it" withSources() withJavadoc())
  }

  lazy val parserCombinators: Seq[ModuleID] = Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
  )
}