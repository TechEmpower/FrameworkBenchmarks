name := "http4s"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.7"

com.github.retronym.SbtOneJar.oneJarSettings

val http4sVersion = "0.8.3"

libraryDependencies ++= Seq(
	"org.http4s" %% "http4s-blazeserver" % http4sVersion,
	"org.http4s" %% "http4s-dsl" % http4sVersion,
	"org.http4s" %% "http4s-argonaut" % http4sVersion
)

mainClass in oneJar := Some("WebServer")

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
