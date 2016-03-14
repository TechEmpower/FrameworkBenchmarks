organization := "com.typesafe.akka"

name := "akka-http-benchmark"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.7"

resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http-experimental" % "2.4.2",
  "com.typesafe.akka" %% "akka-http-spray-json-experimental" % "2.4.2",
  "mysql" % "mysql-connector-java" % "5.1.38",
  "org.apache.commons" % "commons-dbcp2" % "2.1",
  "org.scalatra.scalate" %% "scalate-core" % "1.7.0",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)

assemblyJarName in assembly := "akka-http-benchmark.jar"

mainClass in assembly := Some("com.typesafe.akka.http.benchmark.Main")
