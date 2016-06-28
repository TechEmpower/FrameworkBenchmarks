import sbt._
import Keys._
import sbtassembly.AssemblyPlugin._
//import AssemblyKeys._

object Bench extends Build {
  lazy val project = Project(
    "bench", 
    file("."),
    settings = Defaults.defaultSettings ++ assemblySettings ++ Seq(
      scalaVersion := "2.11.7",
      version := "1.0.0",
      name := "bench",
      libraryDependencies ++= Seq(
        "net.databinder" %% "unfiltered-netty-server" % "0.8.4",
        "net.databinder.dispatch" %% "dispatch-core" % "0.11.2",
        "net.databinder" %% "unfiltered-json4s" % "0.8.4",
        "net.databinder" %% "unfiltered-specs2" % "0.8.4" % "test",
        "org.clapper" %% "avsl" % "1.0.2",
        "org.json4s" %% "json4s-jackson" % "3.3.0",
        "com.typesafe.slick" %% "slick" % "3.1.1",
        "mysql" % "mysql-connector-java" % "5.1.38",
        "com.jolbox" % "bonecp" % "0.7.1.RELEASE",
        "com.typesafe" % "config" % "1.0.0"
      )
    )
  )
}
