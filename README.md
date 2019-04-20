# Scala by Backwards

Scala module that not only provides useful reusable functionality, but also acts a Scala learning tool, specifically around **Functional Programming** in Scala.

Of course you can simply clone/fork this module.
However, this module is also published on [JitPack](https://jitpack.io), where the following dependency can be added to your project:

```scala
lazy val backwards: Seq[ModuleID] = {
  val version = "1.0.8"

  Seq(
    "com.github.backwards-limited" % "scala-backwards" % version % "test, it" classifier "tests",
    "com.github.backwards-limited" % "scala-backwards" % version % "test, it" classifier "it"
  ) ++ Seq(
    "com.github.backwards-limited" % "scala-backwards" % version
  )
}
```

The above dependency declaration includes source, test and integration test code.

You will also need to add a **resolver** to **JitPack**:

```scala
resolvers ++= Seq(
  <your current resolvers>,
  "jitpack" at "https://jitpack.io"
)
```

To see an example of the above, take a look at [Kafka Backwards](https://github.com/backwards-limited/kafka-backwards).

## Table of Contents

- [Setup](docs/setup.md)

- [SBT](docs/sbt.md)

- [Release](docs/release.md)

- Modules

  - [Functional Programming: Custom code for Category Theory](docs/fp.md)
  
  - [Variance: Proofs](docs/variance.md)

  - [FS2: Functional Streaming for Scala](docs/fs2.md)

  - [Cats](docs/cats.md)