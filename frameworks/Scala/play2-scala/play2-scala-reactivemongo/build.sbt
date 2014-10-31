name := "play2-scala-reactivemongo"

version := "1.0-SNAPSHOT"

// Rich Dougherty 31 Oct 2014: We need to use Scala
// 2.10 because play2-reactivemongo 0.8 hasn't been
// compiled for Scala 2.11.
scalaVersion := "2.11.4"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

libraryDependencies ++= Seq(
  "org.reactivemongo" %% "play2-reactivemongo" % "0.10.5.0.akka23"
)