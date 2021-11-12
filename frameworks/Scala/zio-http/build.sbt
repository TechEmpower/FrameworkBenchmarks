name := "zio-http"

version := "1.0.0"
scalaVersion := "2.13.6"

lazy val zhttp = ProjectRef(uri(s"https://github.com/dream11/zio-http.git#6f3e008af1be8c733e2701dbb9da75ca47544328"), "zhttp")
lazy val root  = (project in file("."))
  .settings(
    name := "helloExample",
    libraryDependencies ++=
      Seq(
        "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core"   % "2.9.1",
        "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.9.1" % "compile-internal"
        // "io.d11"                                 % "zhttp"
      ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
  .dependsOn(zhttp)
