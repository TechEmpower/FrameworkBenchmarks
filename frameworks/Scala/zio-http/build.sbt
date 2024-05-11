name := "zio-http"
version := "1.0.0"
scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies += "dev.zio" %% "zio-http" % "3.0.0-RC6",
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    assembly / assemblyMergeStrategy  := {
      case x if x.contains("io.netty.versions.properties") => MergeStrategy.discard
      case x =>
        val oldStrategy = (assembly / assemblyMergeStrategy).value
        oldStrategy(x)
    }
  )
