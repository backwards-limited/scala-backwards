import sbt.*

lazy val IntegrationTestConfig: Configuration =
  config("integration") extend Test

lazy val integrationTestSettings: Seq[Def.Setting[?]] =
  inConfig(IntegrationTestConfig)(Defaults.testSettings)

lazy val root: Project =
  project("scala-backwards", file("."))
    .settings(description := "Scala by Backwards")
    .aggregate(main, macros)

lazy val codeGen: TaskKey[Unit] =
  taskKey[Unit]("Generate my file")

// codeGen := (runMain in Compile).toTask(" tech.backwards.macros.LetsGo").value

lazy val macros: Project =
  project("macros", file("macros"))
    .settings(
      codeGen := (Compile / runMain).toTask("tech.backwards.macros.LetsGo").value
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

lazy val main: Project =
  project("main", file("main"))
    .dependsOn(macros)
    .settings(Test / javaOptions ++= Seq("-Dconfig.resource=application.test.conf"))

def project(id: String, base: File): Project =
  Project(id, base)
    .enablePlugins(JavaAppPackaging, CalibanPlugin)
    .configs(IntegrationTestConfig)
    .settings(
      scalaVersion := BuildProperties("scala.version"),
      sbtVersion := BuildProperties("sbt.version"),
      organization := "tech.backwards",
      name := id,
      autoStartServer := false,
      evictionErrorLevel := Level.Info,
      versionScheme := Some("early-semver"),
      addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.3" cross CrossVersion.full),
      addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
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
        // "-P:kind-projector:underscore-placeholders" // Can use _ instead of * when defining anonymous type lambdas
        // "-Xfatal-warnings"
        // "-Ywarn-value-discard"
      ),
      libraryDependencies ++= Dependencies(),
      excludeDependencies ++= Seq(ExclusionRule("org.typelevel", "munit-cats-effect-2_2.13")),
      dependencyCheckAssemblyAnalyzerEnabled := Some(false),
      exportJars := true,
      fork := true,
      Test / publishArtifact := true,
      testFrameworks := Seq(TestFrameworks.MUnit, TestFrameworks.WeaverTestCats, TestFrameworks.Specs2, TestFrameworks.ScalaTest), //new TestFramework("weaver.framework.CatsEffect")
      integrationTestSettings
    )