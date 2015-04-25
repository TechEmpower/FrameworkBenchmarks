name := "http4s"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.6"

com.github.retronym.SbtOneJar.oneJarSettings

libraryDependencies ++= Seq(
	"org.http4s" %% "http4s-blazeserver" % "0.6.2",
	"org.http4s" %% "http4s-dsl" % "0.6.2",
	"org.http4s" %% "http4s-argonaut" % "0.6.2"
)

mainClass in oneJar := Some("WebServer")

resolvers += "Bintray" at "http://dl.bintray.com/pchiusano/maven/"
