name := "play2-java-jooq-hikaricp"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayJava, PlayNettyServer)

scalaVersion := "2.12.6"

val jOOQVersion = "3.10.3"

libraryDependencies ++= Seq(
  guice,
  javaJdbc,
  "mysql" % "mysql-connector-java" % "5.1.45",
  "org.jooq" % "jooq" % jOOQVersion,
)
