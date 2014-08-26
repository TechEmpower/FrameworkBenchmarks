name := "play-java-jpa"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayJava)

scalaVersion := "2.11.2"

libraryDependencies ++= Seq(
  javaJdbc,
  javaJpa,
  "mysql" % "mysql-connector-java" % "5.1.32",
  "org.hibernate" % "hibernate-entitymanager" % "4.3.6.Final"
  )

dependencyOverrides += "com.jolbox" % "bonecp" % "0.8.0.RELEASE"
