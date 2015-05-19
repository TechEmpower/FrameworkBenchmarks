organization := "com.typesafe.akka"

name := "akka-http-benchmark"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.6"

resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http-core-experimental" % "1.0-RC2",
  "com.typesafe.akka" %% "akka-http-scala-experimental" % "1.0-RC2",
  "com.typesafe.akka" %% "akka-http-spray-json-experimental" % "1.0-RC2",
  "mysql" % "mysql-connector-java" % "5.1.35",
  "org.apache.commons" % "commons-dbcp2" % "2.1",
  "org.scalatra.scalate" %% "scalate-core" % "1.7.0",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)

assemblyJarName in assembly := "akka-http-benchmark.jar"

mainClass in assembly := Some("com.typesafe.akka.http.benchmark.Main")
