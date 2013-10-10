import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

  val appName         = "play-activate-mysql"
  val appVersion      = "1.0-SNAPSHOT"

  val activateVersion = "1.4.1"
  val activateCore = "net.fwbrasil" %% "activate-core" % activateVersion
  val activateJdbc = "net.fwbrasil" %% "activate-jdbc" % activateVersion
  val activatePlay = "net.fwbrasil" %% "activate-play" % "1.4.1-PLAY-2.1.2-RC1"

  val mysql = "mysql" % "mysql-connector-java" % "5.1.16"

  val appDependencies = Seq(
    jdbc,
    mysql,
    activateCore,
    activateJdbc,
    activatePlay
  )

  val main = play.Project(appName, appVersion, appDependencies).settings(
	resolvers ++= Seq("fwbrasil.net" at "http://fwbrasil.net/maven/")
  )

}
