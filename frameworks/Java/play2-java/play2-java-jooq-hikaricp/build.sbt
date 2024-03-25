name := "play2-java-jooq-hikaricp"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayMinimalJava, PlayNettyServer).disablePlugins(PlayFilters)

scalaVersion := "2.13.12"

val jOOQVersion = "3.19.3"

libraryDependencies ++= Seq(
  guice,
  javaJdbc,
  "com.mysql" % "mysql-connector-j" % "8.3.0",
  "org.jooq" % "jooq" % jOOQVersion,
)
