enablePlugins(JavaAppPackaging)

organization := "com.typesafe.akka"

name := "akka-http-benchmark"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.12.5"

resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % "10.1.7",
  "com.typesafe.akka" %% "akka-stream" % "2.5.20",
  "de.heikoseeberger" %% "akka-http-jsoniter-scala" % "1.23.0",
  "mysql" % "mysql-connector-java" % "5.1.47",
  "com.zaxxer" % "HikariCP" % "3.3.0",
  "org.scalatra.scalate" %% "scalate-core" % "1.8.0",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)

mainClass in Compile := Some("com.typesafe.akka.http.benchmark.Main")
