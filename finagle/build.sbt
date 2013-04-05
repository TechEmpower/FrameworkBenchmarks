name := "finagle"

organization := "com.falmarri"

scalaVersion := "2.10.0"

version := "1.0"

libraryDependencies += "com.twitter" % "finagle-http_2.10" % "6.+"

libraryDependencies += "com.fasterxml.jackson.module" % "jackson-module-scala_2.10" % "2.+"

libraryDependencies += "com.typesafe.slick" % "slick_2.10" % "1.0.0"

libraryDependencies += "mysql" % "mysql-connector-java" % "5.1.24"
