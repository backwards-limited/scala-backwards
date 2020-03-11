import sbt._

object Dependencies {
  def apply(): Seq[ModuleID] = Seq(
    scalaReflect, scalatest, scalacheck, scalacheckShapeless, testcontainers, scalaTestContainers, scribe, pureConfig, pprint,
    cats, mouse, simulacrum, refined, monocle, shapeless,
    monix, fs2, scalaUri, betterFiles
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
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.4" withSources() withJavadoc()
  )
  
  lazy val testcontainers: Seq[ModuleID] = Seq(
    "org.testcontainers" % "testcontainers" % "1.12.5" % "test, it" withSources() withJavadoc()
  )

  lazy val scalaTestContainers: Seq[ModuleID] = {
    val group = "com.dimafeng"
    val version = "1.0.0-alpha1"

    Seq(
      "testcontainers-scala-scalatest", "testcontainers-scala-kafka", "testcontainers-scala-mysql"
    ).map(group %% _ % version % "test, it" withSources() withJavadoc())
  }

  lazy val scribe: Seq[ModuleID] = Seq(
    "com.outr" %% "scribe" % "2.7.10" withSources() withJavadoc()
  )
  
  lazy val pureConfig: Seq[ModuleID] = {
    val group = "com.github.pureconfig"
    val version = "0.12.2"

    Seq("pureconfig").map(group %% _ % version withSources() withJavadoc())
  }

  lazy val pprint: Seq[ModuleID] = Seq(
    "com.lihaoyi" %% "pprint" % "0.5.9"
  )

  lazy val cats: Seq[ModuleID] = {
    val group = "org.typelevel"
    val version = "2.1.1"

    Seq(
      "cats-core", "cats-effect", "cats-free"
    ).map(group %% _ % version withSources() withJavadoc()) ++ Seq(
      "cats-laws", "cats-testkit"
    ).map(group %% _ % version % "test, it" withSources() withJavadoc())
  }

  lazy val mouse: Seq[ModuleID] = Seq(
    "org.typelevel" %% "mouse" % "0.24" withSources() withJavadoc()
  )

  lazy val simulacrum: Seq[ModuleID] = Seq(
    "org.typelevel" %% "simulacrum" % "1.0.0" withSources() withJavadoc()
  )
  
  lazy val refined: Seq[ModuleID] = {
    val group = "eu.timepit"
    val version = "0.9.12"

    Seq(
      "refined", "refined-pureconfig", "refined-cats"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val monocle: Seq[ModuleID] = {
    val group = "com.github.julien-truffaut"
    val version = "2.0.1"

    Seq(
      "monocle-law"
    ).map(group %% _ % version % "test, it" withSources() withJavadoc()) ++ Seq(
      "monocle-core", "monocle-macro", "monocle-generic"
    ).map(group %% _ % version withSources() withJavadoc())
  }

  lazy val shapeless: Seq[ModuleID] = Seq(
    "com.chuusai" %% "shapeless" % "2.3.3"
  )

  lazy val monix: Seq[ModuleID] = {
    Seq(
      "io.monix" %% "monix" % "3.1.0" withSources() withJavadoc()
    ) ++ Seq(
      "io.monix" %% "monix-kafka-11" % "1.0.0-RC5" withSources() withJavadoc()
    )
  }

  lazy val fs2: Seq[ModuleID] = {
    val group = "co.fs2"
    val version = "2.2.2"

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