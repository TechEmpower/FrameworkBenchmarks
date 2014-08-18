import sbt._
import Keys._

object build extends Build {

  lazy val root = Project("lift-stateless", file(".")) settings (Deploy.deploySettings:_*)
}
