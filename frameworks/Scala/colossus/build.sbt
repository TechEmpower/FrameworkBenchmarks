name := """colossus-example"""

version := "0.3.0"

scalaVersion := "2.11.7"

com.github.retronym.SbtOneJar.oneJarSettings

mainClass in oneJar := Some("example.Main")

libraryDependencies ++= Seq(
  "com.tumblr" %% "colossus" % "0.7.1-RC1",
  "org.json4s" %% "json4s-jackson" % "3.3.0"  
)
