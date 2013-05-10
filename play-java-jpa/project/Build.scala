import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "play-java-jpa"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      // Add your project dependencies here,
      javaCore,
      javaJdbc,
      javaJpa,
      "mysql" % "mysql-connector-java" % "5.1.22",
      "org.hibernate" % "hibernate-entitymanager" % "4.2.1.Final"
    )

    val main = play.Project(appName, appVersion, appDependencies).settings(
      // Add your own project settings here 
    )

}
