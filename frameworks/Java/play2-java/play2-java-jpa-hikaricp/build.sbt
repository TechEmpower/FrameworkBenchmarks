name := "play2-java-jpa-hikaricp"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayJava)

scalaVersion := "2.12.4"

libraryDependencies ++= Seq(
  guice,
  javaJpa,
  "mysql" % "mysql-connector-java" % "5.1.44",
  "org.hibernate" % "hibernate-core" % "5.2.12.Final"
)

PlayKeys.externalizeResources := false
