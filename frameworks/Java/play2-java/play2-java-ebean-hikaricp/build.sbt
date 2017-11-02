name := "play2-java-ebean-hikaricp"
version := "1.0-SNAPSHOT"
scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  guice,
  javaJdbc,
  "mysql" % "mysql-connector-java" % "5.1.38"
)

routesGenerator := InjectedRoutesGenerator

lazy val root = (project in file(".")).enablePlugins(PlayJava, PlayEbean)

