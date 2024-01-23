name := "play2-java-ebean-hikaricp"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayMinimalJava, PlayEbean, PlayNettyServer).disablePlugins(PlayFilters)

scalaVersion := "2.13.12"

libraryDependencies ++= Seq(
  guice,
  javaJdbc,
  "com.mysql" % "mysql-connector-j" % "8.3.0",
)
