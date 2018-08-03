enablePlugins(JavaAppPackaging)

organization := "com.typesafe.akka"

name := "akka-http-benchmark"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.12.6"

resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % "10.1.3",
  "com.typesafe.akka" %% "akka-stream" % "2.5.14",
  "de.heikoseeberger" %% "akka-http-jsoniter-scala" % "1.21.0",
  "mysql" % "mysql-connector-java" % "5.1.46",
  "com.zaxxer" % "HikariCP" % "2.7.9",
  "org.scalatra.scalate" %% "scalate-core" % "1.8.0",
  "org.scalatest" %% "scalatest" % "3.0.4" % "test"
)

mainClass in Compile := Some("com.typesafe.akka.http.benchmark.Main")
