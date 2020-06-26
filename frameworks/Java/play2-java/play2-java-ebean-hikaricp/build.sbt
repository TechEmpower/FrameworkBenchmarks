name := "play2-java-ebean-hikaricp"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayMinimalJava, PlayEbean, PlayNettyServer).disablePlugins(PlayFilters)

scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
  guice,
  javaJdbc,
  "mysql" % "mysql-connector-java" % "8.0.19"
)
