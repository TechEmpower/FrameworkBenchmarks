name := "finagle"

organization := "com.falmarri"

scalaVersion := "2.10.0"

version := "1.0"

libraryDependencies ++= Seq(
                "com.twitter" %% "finagle-http" % "6.+",
                "com.twitter" %% "finagle-mysql" % "6.+",
                "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.+"
                )
