name := "play2-java-jpa-bonecp"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.4"

lazy val root = (project in file(".")).enablePlugins(PlayJava)

libraryDependencies ++= Seq(
  javaJdbc,
  javaJpa,
  "mysql" % "mysql-connector-java" % "5.1.33",
  "org.hibernate" % "hibernate-entitymanager" % "4.3.6.Final"
  )

dependencyOverrides += "com.jolbox" % "bonecp" % "0.8.0.RELEASE"
