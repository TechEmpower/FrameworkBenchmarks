name := """colossus-example"""

version := "0.2.0"

scalaVersion := "2.11.6"

com.github.retronym.SbtOneJar.oneJarSettings

mainClass in oneJar := Some("example.Main")

libraryDependencies ++= Seq(
  "com.tumblr" %% "colossus" % "0.6.4-RC1",
  "net.liftweb" %% "lift-json" % "2.6-RC1"
)
