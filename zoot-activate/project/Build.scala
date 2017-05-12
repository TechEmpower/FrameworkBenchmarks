import sbt._
import Keys._

object Build extends Build {

    val activateVersion = "1.6.1"
    val activateCore = "net.fwbrasil" %% "activate-core" % activateVersion
    val activateJdbcAsync = "net.fwbrasil" %% "activate-jdbc-async" % activateVersion
    val activateFinagleMysql = "net.fwbrasil" %% "activate-finagle-mysql" % activateVersion
    val activateMongoAsync = "net.fwbrasil" %% "activate-mongo-async" % activateVersion
    val activateTest = "net.fwbrasil" %% "activate-test" % activateVersion % "test"

    val zootFinagle = "net.fwbrasil" %% "zoot-finagle" % "0.12"
    val zootSpray = "net.fwbrasil" %% "zoot-spray" % "0.12"

    val junit = "junit" % "junit" % "4.11" % "test"
    val scalaTest = "org.scalatest" %% "scalatest" % "2.1.6" % "test"

    val playJson = "com.typesafe.play" %% "play-json" % "2.2.2"
    val mustacheJava = "com.github.spullara.mustache.java" % "compiler" % "0.8.15"

    val reactiveMongo = "org.reactivemongo" %% "reactivemongo" % "0.10.5.akka23-SNAPSHOT"

    lazy val benchmark =
        Project(
            id = "zoot-activate",
            base = file("."),
            settings = commonSettings ++ Seq(
                libraryDependencies ++=
                    Seq(zootFinagle, zootSpray, activateCore, activateJdbcAsync,
                        junit, activateTest, scalaTest, playJson, mustacheJava,
                        activateFinagleMysql, activateMongoAsync, reactiveMongo)
            )
        )

    val customResolvers = Seq(
        "fwbrasil" at "http://fwbrasil.net/maven/",
        "jboss" at "https://repository.jboss.org/nexus/content/groups/public",
        "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
        "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/",
        "spray repo" at "http://repo.spray.io/"
    )

    def commonSettings =
        Defaults.defaultSettings ++ Seq(
            resolvers ++= customResolvers,
            organization := "benchmark",
            version := "1.0-SNAPSHOT",
            scalaVersion := "2.10.3",
            parallelExecution in Test := false
        )
}
