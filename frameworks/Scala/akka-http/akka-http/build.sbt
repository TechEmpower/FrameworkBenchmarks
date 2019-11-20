enablePlugins(JavaAppPackaging)

organization := "com.typesafe.akka"

name := "akka-http-benchmark"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.13.0"

resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % "10.1.8",
  "com.typesafe.akka" %% "akka-stream" % "2.5.23",
  "de.heikoseeberger" %% "akka-http-jsoniter-scala" % "1.27.0",
  "mysql" % "mysql-connector-java" % "8.0.18",
  "com.zaxxer" % "HikariCP" % "3.3.1",
  "org.scalatra.scalate" %% "scalate-core" % "1.9.4",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test"
)

mainClass in Compile := Some("com.typesafe.akka.http.benchmark.Main")
