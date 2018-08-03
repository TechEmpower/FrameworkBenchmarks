name := "play2-scala-slick"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala, PlayNettyServer)

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
  guice,
  "com.typesafe.play" %% "play-slick" % "3.0.3",
  "mysql" % "mysql-connector-java" % "5.1.45",
  filters
)
