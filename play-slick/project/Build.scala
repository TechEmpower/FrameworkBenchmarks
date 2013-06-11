import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

  val appName         = "play-slick"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    jdbc,
    anorm,
    "mysql" % "mysql-connector-java" % "5.1.22",
    "com.typesafe.play" %% "play-slick" % "0.3.2" 
  )

  val main = play.Project(appName, appVersion, appDependencies).settings(
  )

}
