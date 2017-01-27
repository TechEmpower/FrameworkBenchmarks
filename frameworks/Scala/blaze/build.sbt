name := "blaze"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.8"

com.github.retronym.SbtOneJar.oneJarSettings

val blazeVersion = "0.13.0"

libraryDependencies ++= Seq(
	"org.http4s" %% "blaze-http" % blazeVersion,
	"com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.8.4"
)

mainClass in oneJar := Some("blaze.techempower.benchmark.Main")

