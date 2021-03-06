import sbt._

ThisBuild / evictionErrorLevel := Level.Info

lazy val root = project("scala-backwards", file("."))
  .settings(description := "Scala by Backwards")
  .aggregate(main, macros)

lazy val codeGen = taskKey[Unit]("Generate my file")

// codeGen := (runMain in Compile).toTask(" com.backwards.macros.LetsGo").value

lazy val macros = project("macros", file("macros"))
  .settings(
    codeGen := (Compile / runMain).toTask(" com.backwards.macros.LetsGo").value

    /*Compile / sourceGenerators += Def.task {
      streams.value.log.info("===============> yo")
      /*val file = (Compile / sourceManaged).value / "demo" / "Test.scala"
      IO.write(file, """object Test extends App { println("Hi") }""")
      Seq(file)*/

      example
      //val file = (Compile / scalaSource).value / "com" / "backwards" / "macros" / "LetsGo.scala"
      //IO.write(file, """object Test extends App { println("Hi") }""")
      //Seq(file)

    }.taskValue*/
  )

lazy val main = project("main", file("main"))
  .dependsOn(macros)
  .settings(Test / javaOptions ++= Seq("-Dconfig.resource=application.test.conf"))

def project(id: String, base: File): Project =
  Project(id, base)
    .enablePlugins(JavaAppPackaging)
    .configs(IntegrationTest extend Test)
    .settings(inConfig(IntegrationTest extend Test)(Defaults.testSettings))
    .settings(Defaults.itSettings)
    .settings(
      resolvers ++= Seq(
        Resolver sonatypeRepo "releases",
        Resolver sonatypeRepo "snapshots",
        "jitpack" at "https://jitpack.io",
        "Artima Maven Repository" at "https://repo.artima.com/releases"
      ),
      scalaVersion := BuildProperties("scala.version"),
      sbtVersion := BuildProperties("sbt.version"),
      organization := "com.backwards",
      name := id,
      autoStartServer := false,
      addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.0" cross CrossVersion.full),
      addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
      libraryDependencies ++= Dependencies(),
      exportJars := true,
      scalacOptions ++= Seq(
        "-encoding", "utf8",
        "-deprecation",
        "-unchecked",
        "-language:implicitConversions",
        "-language:higherKinds",
        "-language:existentials",
        "-language:postfixOps",
        "-Ymacro-annotations",
        "-Yrangepos",
        "-P:kind-projector:underscore-placeholders" // Can use _ instead of * when defining anonymous type lambdas
        //"-Xfatal-warnings"
        // "-Ywarn-value-discard"
      ),
      fork := true,
      Test / publishArtifact := true,
      IntegrationTest / publishArtifact := true,
      addArtifact(IntegrationTest / packageBin / artifact, IntegrationTest / packageBin).settings
    )