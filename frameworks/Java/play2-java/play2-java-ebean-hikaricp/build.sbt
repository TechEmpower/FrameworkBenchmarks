name := "play2-java-ebean-hikaricp"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayMinimalJava, PlayEbean, PlayNettyServer).disablePlugins(PlayFilters)

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  guice,
  javaJdbc,
  "mysql" % "mysql-connector-java" % "5.1.47"
)
