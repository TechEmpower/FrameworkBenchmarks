import sbt._
import Keys._

object ApplicationBuild extends Build {

  val appName         = "play-scala-mongodb"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    "org.reactivemongo" %% "play2-reactivemongo" % "0.9" exclude("org.scala-stm", "scala-stm_2.10.0")
  )

  val main = play.Project(appName, appVersion, appDependencies).settings(
  )

}
