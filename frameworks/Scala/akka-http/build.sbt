val akkaV = "2.5.1"
val akkaHttpV = "10.0.6"

organization := "com.typesafe.akka"

name := "akka-http-benchmark"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.12.2"

resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % akkaHttpV,
  "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpV,
  "com.typesafe.akka" %% "akka-stream" % akkaV,
  "mysql" % "mysql-connector-java" % "5.1.38",
  "com.zaxxer" % "HikariCP" % "2.5.1",
  "org.scalatra.scalate" %% "scalate-core" % "1.8.0",
  "org.scalatest" %% "scalatest" % "3.0.3" % "test"
)

assemblyJarName in assembly := "akka-http-benchmark.jar"

mainClass in assembly := Some("com.typesafe.akka.http.benchmark.Main")
