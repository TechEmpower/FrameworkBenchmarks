name := """youi-server"""

version := "1.0"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "io.youi" %% "youi-server-undertow" % "0.10.3",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-core" % "0.39.0",
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "0.39.0" % Provided
)
