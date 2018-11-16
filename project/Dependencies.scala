import sbt._

object Dependencies {
  lazy val dependencies: Seq[ModuleID] =
    Seq(scalatest, scalactic, scalaReflect, shapeless, simulacrum
    ).flatten

  lazy val scalatest: Seq[ModuleID] = Seq(
    "org.scalatest" %% "scalatest" % "3.0.5" % "test, it"
  )

  lazy val scalactic: Seq[ModuleID] = Seq(
    "org.scalactic" %% "scalactic" % "3.0.1" % "test, it"
  )

  lazy val scalaReflect: Seq[ModuleID] = Seq(
    "org.scala-lang" % "scala-reflect" % BuildProperties("scala.version")
  )

  lazy val shapeless: Seq[ModuleID] = Seq(
    "com.chuusai" %% "shapeless" % "2.3.3"
  )

  lazy val simulacrum: Seq[ModuleID] = Seq(
    "com.github.mpilquist" %% "simulacrum" % "0.10.0"
  )
}