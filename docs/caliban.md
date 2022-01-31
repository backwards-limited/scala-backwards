# Caliban

## Caliban Client IT Example

Nice [example](https://medium.com/@ghostdogpr/caliban-client-a-type-safe-graphql-client-for-scala-and-scala-js-718aa42c5ef7) by Pierre Ricadat.

```shell
calibanGenClient <schemaPath> <outputPath> <?scalafmtPath>
```

- schemaPath is the path to the file containing the GraphQL schema
- outputPath is the path to the file that should be created with the generated code
- scalafmtPath is the path to your ScalaFmt config if you want the generated code to be formatted according to your own rules.
  It is optional and the plugin will look at a potential .scalafmt.conf file at the root of your project by default

Run:
```shell
sbt "calibanGenClient main/src/it/resources/bahn.graphql main/src/it/scala/com/backwards/graphql/TrainClient.scala --splitFiles true"
```