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
    "org.scalatest" %% "scalatest" % "3.1.0" % "test, it" withSources() withJavadoc()
  )

  lazy val scalacheck: Seq[ModuleID] = Seq(
    "org.scalacheck" %% "scalacheck" % "1.14.3" % "test, it" withSources() withJavadoc()
  )

  lazy val scalacheckShapeless: Seq[ModuleID] = Seq(
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.3" withSources() withJavadoc()
  )
  
  lazy val testcontainers: Seq[ModuleID] = Seq(
    "org.testcontainers" % "testcontainers" % "1.12.4" % "test, it" withSources() withJavadoc()
  )
  
  lazy val airframe: Seq[ModuleID] = Seq(
    "org.wvlet.airframe" %% "airframe-log" % "19.12.4"
  )

  lazy val logging: Seq[ModuleID] = Seq(
    "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
    "org.slf4j" % "log4j-over-slf4j" % "1.7.30",
    "ch.qos.logback" % "logback-classic" % "1.2.3"
  )
  
  lazy val pureConfig: Seq[ModuleID] = {
    val group = "com.github.pureconfig"
    val version = "0.12.2"

    Seq("pureconfig").map(group %% _ % version withSources() withJavadoc())
  }

  lazy val simulacrum: Seq[ModuleID] = Seq(
    "com.github.mpilquist" %% "simulacrum" % "0.19.0" withSources() withJavadoc()
  )
  
  lazy val refined: Seq[ModuleID] = {
    val group = "eu.timepit"
    val version = "0.9.10"

    Seq(
      "refined", "refined-pureconfig", "refined-cats"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val monocle: Seq[ModuleID] = {
    val group = "com.github.julien-truffaut"
    val version = "2.0.0"

    Seq(
      "monocle-law"
    ).map(group %% _ % version % "test, it" withSources() withJavadoc()) ++ Seq(
      "monocle-core", "monocle-macro", "monocle-generic"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val shapeless: Seq[ModuleID] = Seq(
    "com.chuusai" %% "shapeless" % "2.3.3" withSources() withJavadoc()
  )

  lazy val cats: Seq[ModuleID] = {
    val group = "org.typelevel"
    val version = "2.0.0"

    Seq(
      "cats-core", "cats-effect"
    ).map(group %% _ % version withSources() withJavadoc()) ++ Seq(
      "cats-laws", "cats-testkit"
    ).map(group %% _ % version % "test, it" withSources() withJavadoc())
  }
  
  lazy val mouse: Seq[ModuleID] = Seq(
    "org.typelevel" %% "mouse" % "0.24" withSources() withJavadoc()
  )

  lazy val monix: Seq[ModuleID] = Seq(
    "io.monix" %% "monix" % "3.1.0" withSources() withJavadoc()
  )

  lazy val fs2: Seq[ModuleID] = {
    val group = "co.fs2"
    val version = "2.1.0"

    Seq(
      "fs2-core", "fs2-io", "fs2-reactive-streams"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val scalaUri: Seq[ModuleID] = Seq(
    "io.lemonlabs" %% "scala-uri" % "1.5.1" withSources() withJavadoc()
  )
  
  lazy val betterFiles: Seq[ModuleID] = Seq(
    "com.github.pathikrit" %% "better-files" % "3.8.0" withSources() withJavadoc()
  )
}