import sbt._

object Dependencies {
  lazy val dependencies: Seq[ModuleID] =
    Seq(scalaReflect, scalatest, cats, monocle, shapeless, simulacrum, fs2
    ).flatten

  lazy val scalaReflect: Seq[ModuleID] = Seq(
    "org.scala-lang" % "scala-reflect" % BuildProperties("scala.version")
  )
  
  lazy val scalatest: Seq[ModuleID] = Seq(
    "org.scalatest" %% "scalatest" % "3.0.5" % "test, it"
  )

  lazy val cats: Seq[ModuleID] = {
    val version = "1.4.0"

    Seq(
      "org.typelevel" %% "cats-laws",
      "org.typelevel" %% "cats-testkit"
    ).map(_ % version % "test, it") ++ Seq(
      "org.typelevel" %% "cats-core"
    ).map(_ % version) ++ Seq(
      "org.typelevel" %% "cats-effect" % "1.0.0"
    )
  }

  lazy val monocle: Seq[ModuleID] = {
    val version = "1.5.0"

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

  lazy val simulacrum: Seq[ModuleID] = Seq(
    "com.github.mpilquist" %% "simulacrum" % "0.14.0"
  )

  lazy val fs2: Seq[ModuleID] = {
    val version = "1.0.0"
    
    Seq(
      "co.fs2" %% "fs2-core" % version,
      "co.fs2" %% "fs2-io" % version,
      "co.fs2" %% "fs2-reactive-streams" % version
    )
  }
}