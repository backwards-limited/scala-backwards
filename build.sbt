import com.scalapenos.sbt.prompt.SbtPrompt.autoImport._
import Dependencies._
import sbt._

lazy val IT = config("it") extend Test

lazy val root = project("scala-backwards", file("."))
  .settings(description := "Scala by Backwards")
  .settings(javaOptions in Test ++= Seq("-Dconfig.resource=application.test.conf"))

def project(id: String, base: File): Project =
  Project(id, base)
    .enablePlugins(JavaAppPackaging)
    .configs(IT)
    .settings(inConfig(IT)(Defaults.testSettings))
    .settings(Defaults.itSettings)
    .settings(promptTheme := com.scalapenos.sbt.prompt.PromptThemes.ScalapenosTheme)
    .settings(
      resolvers ++= Seq(
        Resolver.sonatypeRepo("releases"),
        Resolver.sonatypeRepo("snapshots"),
        Resolver.bintrayRepo("cakesolutions", "maven"),
        "Artima Maven Repository" at "http://repo.artima.com/releases",
        "jitpack" at "https://jitpack.io",
        "Confluent Platform Maven" at "http://packages.confluent.io/maven/"
      ),
      scalaVersion := BuildProperties("scala.version"),
      sbtVersion := BuildProperties("sbt.version"),
      organization := "com.backwards",
      name := id,
      autoStartServer := false,
      triggeredMessage := Watched.clearWhenTriggered,
      addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.10"),
      addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full),
      libraryDependencies ++= dependencies,
      fork in Test := true,
      fork in IT := true,
      fork in run := true,
      scalacOptions ++= Seq("-Ypartial-unification"),
      publishArtifact in Test := true,
      publishArtifact in IT := true,
      addArtifact(artifact in (IT, packageBin), packageBin in IT).settings
    )