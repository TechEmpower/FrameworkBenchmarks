name := "zio-http"
version := "1.0.0"
scalaVersion := "2.13.6"
lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++=
      Seq(
        "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % "2.9.1",
        "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.9.1" % "compile-internal",
        "io.d11"                                 % "zhttp"                 % "1.0.0-RC5",
      ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
  )
