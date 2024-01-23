name := "play2-scala-anorm"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala, PlayNettyServer).disablePlugins(PlayFilters)

scalaVersion := "2.13.3"

libraryDependencies ++= Seq(
  guice,
  jdbc,
  "org.playframework.anorm" %% "anorm" % "2.6.7",
  "mysql" % "mysql-connector-java" % "8.0.21"
)
