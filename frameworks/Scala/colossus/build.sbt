name := """colossus-example"""

version := "0.2.1"

scalaVersion := "2.11.7"

com.github.retronym.SbtOneJar.oneJarSettings

mainClass in oneJar := Some("example.Main")

libraryDependencies ++= Seq(
  "com.tumblr" %% "colossus" % "0.6.6-RC2",
  "net.liftweb" %% "lift-json" % "2.6-RC1"
)
