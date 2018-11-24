import ReleaseTransformations._

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,       // ReleaseStep which performs the initial git checks
  tagRelease,
  // publishArtifacts,        // ReleaseStep which checks whether `publishTo` is properly set up
  setNextVersion,
  commitNextVersion,
  pushChanges                 // ReleaseStep which also checks that an upstream branch is properly configured
)