name := "blaze"

version := "1.0"

scalaVersion := "2.12.5"

val blazeVersion = "0.13.0"

libraryDependencies ++= Seq(
	"org.http4s" %% "blaze-http" % blazeVersion,
	"com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.8.4"
)
