name := "youi-server"
version := "1.0"
scalaVersion := "2.13.3"

libraryDependencies ++= Seq(
  "io.youi" %% "youi-server" % "0.13.17",
  "io.youi" %% "youi-server-undertow" % "0.13.17",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "2.6.2",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.6.2" % "compile-internal"
)

enablePlugins(PackPlugin)