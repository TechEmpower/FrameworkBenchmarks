name := "http4s"

version := "1.0-SNAPSHOT"

scalaVersion := "2.12.2"

enablePlugins(SbtTwirl)

TwirlKeys.templateImports += "http4s.techempower.benchmark._"

com.github.retronym.SbtOneJar.oneJarSettings

val http4sVersion = "0.15.9a"
val circeVersion = "0.7.1"
val doobieVersion = "0.4.1"

libraryDependencies ++= Seq(
	"org.http4s" %% "http4s-blaze-server" % http4sVersion,
	"org.http4s" %% "http4s-dsl" % http4sVersion,
	"org.http4s" %% "http4s-circe" % http4sVersion,
	"org.http4s" %% "http4s-twirl" % http4sVersion,
	"io.circe" %% "circe-core" % circeVersion,
	"io.circe" %% "circe-generic" % circeVersion,
	"io.circe" %% "circe-parser" % circeVersion,
	"org.tpolecat" %% "doobie-core" % doobieVersion,
	"org.tpolecat" %% "doobie-hikari" % doobieVersion,
	"com.zaxxer" %  "HikariCP" % "2.6.1",
	"org.postgresql" % "postgresql" % "9.4.1212",
	"ch.qos.logback" % "logback-classic" % "1.2.2"
)

mainClass in oneJar := Some("http4s.techempower.benchmark.WebServer")

