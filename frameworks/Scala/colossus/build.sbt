name := """colossus-example"""

version := "1.0"

scalaVersion := "2.12.5"

libraryDependencies ++= Seq(
  "com.tumblr" %% "colossus" % "0.11.0-M4",
  "com.github.plokhotnyuk.jsoniter-scala" %% "macros" % "0.3.4"
)
