import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "play-java"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      // Add your project dependencies here,
      javaCore,
      javaJdbc,
      javaEbean,
      "mysql" % "mysql-connector-java" % "5.1.22",
      "org.slf4j" % "slf4j-api" % "1.6.6",
      "com.fasterxml.jackson.core" % "jackson-databind" % "2.1.1"
    )

    val main = play.Project(appName, appVersion, appDependencies).settings(
      // Add your own project settings here 
    )

}
