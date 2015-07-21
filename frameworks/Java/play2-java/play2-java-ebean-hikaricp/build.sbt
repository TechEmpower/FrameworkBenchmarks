name := "play2-java-ebean-hikaricp"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayJava, PlayEbean)

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  javaJdbc,
  "mysql" % "mysql-connector-java" % "5.1.35"
)

routesGenerator := InjectedRoutesGenerator