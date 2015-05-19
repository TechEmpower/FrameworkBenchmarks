name := "play2-java-ebean-bonecp"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayJava)

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  javaJdbc,
  javaEbean,
  "mysql" % "mysql-connector-java" % "5.1.35"
)
