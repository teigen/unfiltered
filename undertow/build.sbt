description := "Undertow server binding module"

unmanagedClasspath in (local("undertow"), Test) <++=
  (fullClasspath in (local("spec"), Compile))

libraryDependencies <++= scalaVersion(v =>
  ("io.undertow" % "undertow-core" % "1.0.0.Beta1") +:
    Common.integrationTestDeps(v)
)
