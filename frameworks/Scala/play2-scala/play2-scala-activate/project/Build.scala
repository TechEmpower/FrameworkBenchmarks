import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "play2-scala-activate"
  val appVersion      = "1.0-SNAPSHOT"

  val activateVersion = "1.4.4"
  val activateCore = "net.fwbrasil" %% "activate-core" % activateVersion
  val activateJdbc = "net.fwbrasil" %% "activate-jdbc" % activateVersion
  val activatePlay = "net.fwbrasil" %% "activate-play" % activateVersion

  val mysql = "mysql" % "mysql-connector-java" % "5.1.16"

  val appDependencies = Seq(
    mysql,
    activateCore,
    activateJdbc,
    activatePlay
  )

  val main = play.Project(appName, appVersion, appDependencies).settings(
	  resolvers ++= Seq("fwbrasil.net" at "http://fwbrasil.net/maven/")
  )

}
