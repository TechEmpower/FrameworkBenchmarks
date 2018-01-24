name := """colossus-example"""

version := "0.4.0"

scalaVersion := "2.12.4"

com.github.retronym.SbtOneJar.oneJarSettings

mainClass in oneJar := Some("example.Main")

libraryDependencies ++= Seq(
  "com.tumblr" %% "colossus" % "0.11.0-M4",
  "com.github.plokhotnyuk.jsoniter-scala" %% "macros" % "0.3.4"
)
