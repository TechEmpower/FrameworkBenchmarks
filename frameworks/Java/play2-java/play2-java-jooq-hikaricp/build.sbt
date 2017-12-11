name := "play2-java-jooq-hikaricp"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayJava, PlayNettyServer)

scalaVersion := "2.12.4"

val jOOQVersion = "3.10.2"

libraryDependencies ++= Seq(
  guice,
  javaJdbc,
  "mysql" % "mysql-connector-java" % "5.1.44",
  "org.jooq" % "jooq" % jOOQVersion,
)
