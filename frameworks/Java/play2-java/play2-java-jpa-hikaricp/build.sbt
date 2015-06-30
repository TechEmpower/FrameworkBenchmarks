name := """play2-java-jpa-hikaricp"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayJava)

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  javaJdbc,
  javaJpa,
  "mysql" % "mysql-connector-java" % "5.1.35",
  "org.hibernate" % "hibernate-entitymanager" % "4.3.10.Final"
)

routesGenerator := InjectedRoutesGenerator

PlayKeys.externalizeResources := false