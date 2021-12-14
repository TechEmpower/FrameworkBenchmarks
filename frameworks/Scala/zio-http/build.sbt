name := "zio-http"

version      := "1.0.0"
scalaVersion := "2.13.7"

lazy val root = (project in file("."))
  .settings(
    name := "helloExample",
    fork := true,
    libraryDependencies ++=
      Seq(
        "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % "2.9.1",
        "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.9.1" % "compile-internal",
        "com.amazonaws"                          % "aws-java-sdk"          % "1.11.500",
        "io.d11" % "zhttp" % "1.0.0.0" from "file:///zhttp/zhttp-1.0.0.0.jar",
      ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
  )
assembly / assemblyMergeStrategy := {
  case PathList("META-INF", "io.netty.versions.properties") => MergeStrategy.discard
  case x if x.contains("io/netty/util")                     => MergeStrategy.last
  case x if x.contains("io/netty/handler")                  => MergeStrategy.last
  case x if x.contains("io/netty/bootstrap")                => MergeStrategy.last
  case x if x.contains("io/netty/buffer")                   => MergeStrategy.last
  case x if x.contains("io/netty/channel")                  => MergeStrategy.last
  case x if x.contains("io/netty/resolver")                 => MergeStrategy.last
  case x                                                    =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}
