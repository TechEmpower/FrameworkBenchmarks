name := "play2-java-jpa-hikaricp"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayJava, PlayNettyServer)

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  guice,
  javaJpa,
  "mysql" % "mysql-connector-java" % "5.1.47",
  "org.hibernate" % "hibernate-core" % "5.4.1.Final"
)

PlayKeys.externalizeResources := false
