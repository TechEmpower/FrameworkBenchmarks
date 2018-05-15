enablePlugins(JavaAppPackaging)

val akkaVersion = "2.5.7"
val akkaHttpVersion = "10.0.11"

organization := "com.typesafe.akka"

name := "akka-http-benchmark"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.12.5"

resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-stream" % akkaVersion,
  "mysql" % "mysql-connector-java" % "5.1.45",
  "com.zaxxer" % "HikariCP" % "2.7.4",
  "org.scalatra.scalate" %% "scalate-core" % "1.8.0",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test"
)

mainClass in Compile := Some("com.typesafe.akka.http.benchmark.Main")
