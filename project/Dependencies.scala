import sbt._

object Dependencies {
  lazy val dependencies: Seq[ModuleID] =
    Seq(
      scalaReflect, scalatest, scalacheck, scalacheckShapeless, testcontainers,
      airframe, logging, pureConfig,
      simulacrum, refined, monocle, shapeless,
      cats, mouse, monix,
      fs2,
      scalaUri, betterFiles
    ).flatten

  lazy val scalaReflect: Seq[ModuleID] = Seq(
    "org.scala-lang" % "scala-reflect" % BuildProperties("scala.version")
  )
  
  lazy val scalatest: Seq[ModuleID] = Seq(
    "org.scalatest" %% "scalatest" % "3.0.8" % "test, it"
  )

  lazy val scalacheck: Seq[ModuleID] = Seq(
    "org.scalacheck" %% "scalacheck" % "1.14.2" % "test, it"
  )

  lazy val scalacheckShapeless: Seq[ModuleID] = Seq(
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.3"
  )
  
  lazy val testcontainers: Seq[ModuleID] = Seq(
    "org.testcontainers" % "testcontainers" % "1.12.2" % "test, it"
  )
  
  lazy val airframe: Seq[ModuleID] = Seq(
    "org.wvlet.airframe" %% "airframe-log" % "19.10.1"
  )

  lazy val logging: Seq[ModuleID] = Seq(
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
    "org.slf4j" % "log4j-over-slf4j" % "1.7.28",
    "ch.qos.logback" % "logback-classic" % "1.2.3"
  )
  
  lazy val pureConfig: Seq[ModuleID] = {
    val version = "0.12.1"

    Seq(
      "com.github.pureconfig" %% "pureconfig"
    ).map(_ % version)
  }

  lazy val simulacrum: Seq[ModuleID] = Seq(
    "com.github.mpilquist" %% "simulacrum" % "0.19.0"
  )
  
  lazy val refined: Seq[ModuleID] = {
    val version = "0.9.10"

    Seq(
      "eu.timepit" %% "refined",
      "eu.timepit" %% "refined-pureconfig",
      "eu.timepit" %% "refined-cats"
    ).map(_ % version)
  }

  lazy val monocle: Seq[ModuleID] = {
    val version = "2.0.0"

    Seq(
      "com.github.julien-truffaut" %% "monocle-law"
    ).map(_ % version % "test, it") ++ Seq(
      "com.github.julien-truffaut" %% "monocle-core",
      "com.github.julien-truffaut" %% "monocle-macro",
      "com.github.julien-truffaut" %% "monocle-generic"
    ).map(_ % version)
  }

  lazy val shapeless: Seq[ModuleID] = Seq(
    "com.chuusai" %% "shapeless" % "2.3.3"
  )

  lazy val cats: Seq[ModuleID] = {
    val version = "2.0.0"

    Seq(
      "org.typelevel" %% "cats-core",
      "org.typelevel" %% "cats-effect"
    ).map(_ % version) ++ Seq(
      "org.typelevel" %% "cats-laws",
      "org.typelevel" %% "cats-testkit"
    ).map(_ % version % "test, it")
  }
  
  lazy val mouse: Seq[ModuleID] = Seq(
    "org.typelevel" %% "mouse" % "0.23"
  )

  lazy val monix: Seq[ModuleID] = Seq(
    "io.monix" %% "monix" % "3.0.0"
  )

  lazy val fs2: Seq[ModuleID] = {
    val version = "2.0.1"

    Seq(
      "co.fs2" %% "fs2-core",
      "co.fs2" %% "fs2-io",
      "co.fs2" %% "fs2-reactive-streams"
    ).map(_ % version)
  }

  lazy val scalaUri: Seq[ModuleID] = Seq(
    "io.lemonlabs" %% "scala-uri" % "1.5.1"
  )
  
  lazy val betterFiles: Seq[ModuleID] = Seq(
    "com.github.pathikrit" %% "better-files" % "3.8.0"
  )
}