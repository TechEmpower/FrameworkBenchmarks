name := """cask-example"""

version := "1.0"

scalaVersion := "2.12.5"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "cask" % "0.1.1",
  "com.github.plokhotnyuk.jsoniter-scala" %% "macros" % "0.21.6"
)
