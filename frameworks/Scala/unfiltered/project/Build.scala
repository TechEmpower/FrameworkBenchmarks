import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._

object Bench extends Build {
  lazy val project = Project(
    "bench", 
    file("."),
    settings = Defaults.defaultSettings ++ assemblySettings ++ Seq(
      scalaVersion := "2.10.1",
      version := "1.0.0",
      name := "bench",
      libraryDependencies ++= Seq(
        "net.databinder" %% "unfiltered-netty-server" % "0.6.8",
        "net.databinder.dispatch" %% "dispatch-core" % "0.9.5",
        "net.databinder" %% "unfiltered-json4s" % "0.6.8",
        "net.databinder" %% "unfiltered-spec" % "0.6.8" % "test",
        "org.clapper" %% "avsl" % "1.0.1",
        "org.json4s" %% "json4s-jackson" % "3.2.3",
        "com.typesafe.slick" %% "slick" % "1.0.0",
        "mysql" % "mysql-connector-java" % "5.1.24",
        "com.jolbox" % "bonecp" % "0.7.1.RELEASE",
        "com.typesafe" % "config" % "1.0.0"
      )
    )
  )
}
