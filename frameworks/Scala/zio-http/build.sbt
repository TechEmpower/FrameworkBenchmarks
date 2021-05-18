name := "zio-http"

version := "1.0.0"
scalaVersion := "2.13.3"
lazy val root = (project in file("."))
  .settings(
    name := "helloExample",
    libraryDependencies ++=
      Seq(
        "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % "2.6.4",
        "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.6.4" % "compile-internal",
        "io.d11"                                 % "zhttp"                 % "1.0.0-RC3.1",
      ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
  )
