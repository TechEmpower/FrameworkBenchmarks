name := "play2-scala-anorm"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala, PlayNettyServer).disablePlugins(PlayFilters)

scalaVersion := "2.13.16"

libraryDependencies ++= Seq(
  guice,
  jdbc,
  "org.playframework.anorm" %% "anorm" % "2.8.1",
  "mysql" % "mysql-connector-java" % "8.0.33"
)
