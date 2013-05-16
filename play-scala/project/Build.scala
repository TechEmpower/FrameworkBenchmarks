import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "play-scala"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    jdbc,
    anorm,
    "mysql" % "mysql-connector-java" % "5.1.22"
  )

  val main = play.Project(appName, appVersion, appDependencies).settings(
  )

}
