import sbt.url

ThisBuild / versionScheme := Some("early-semver")
ThisBuild / organizationName := "davidainslie"
ThisBuild / organizationHomepage := Some(url("https://github.com/backwards-limited/scala-backwards"))
ThisBuild / homepage := Some(url("https://github.com/backwards-limited/scala-backwards"))
ThisBuild / licenses := List("The Unlicense" -> new URL("https://unlicense.org/"))
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishMavenStyle := true
ThisBuild / sonatypeRepository := "https://s01.oss.sonatype.org/service/local"
ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/backwards-limited/scala-backwards"),
    "scm:git@github.backwards-limited/scala-backwards.git"
  )
)

ThisBuild / developers := List(
  Developer(
    id    = "davidainslie",
    name  = "David Ainslie",
    email = "dainslie@gmail.com",
    url   = url("https://github.com/backwards-limited/scala-backwards")
  )
)

ThisBuild / publishTo := sonatypePublishToBundle.value