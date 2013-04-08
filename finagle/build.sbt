name := "finagle"

organization := "com.falmarri"

scalaVersion := "2.10.0"

version := "1.0"

libraryDependencies ++= Seq(
                "com.twitter" % "finagle-http_2.10" % "6.+",
                "com.fasterxml.jackson.module" % "jackson-module-scala_2.10" % "2.+",
                "com.typesafe.slick" % "slick_2.10" % "1.0.0",
                "mysql" % "mysql-connector-java" % "5.1.24",
                "commons-dbcp" % "commons-dbcp" % "1.+"
                )
