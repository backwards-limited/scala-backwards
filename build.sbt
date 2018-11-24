import Dependencies._
import sbt._

lazy val root = project("scala-backwards", file("."))
  .settings(description := "Scala by Backwards")

def project(id: String, base: File): Project =
  Project(id, base)
    .enablePlugins(JavaAppPackaging)
    .configs(IntegrationTest)
    .settings(Defaults.itSettings)
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
      addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9"),
      addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full),
      libraryDependencies ++= dependencies,
      fork in Test := true,
      fork in IntegrationTest := true,
      scalacOptions in (Compile, doc) ++= Seq("-groups", "-implicits"),
      publishArtifact in Test := true,
      publishArtifact in IntegrationTest := true,
      addArtifact(artifact in (IntegrationTest, packageBin), packageBin in IntegrationTest).settings
    )