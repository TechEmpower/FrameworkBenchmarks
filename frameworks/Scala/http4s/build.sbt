name := "http4s"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.7"

enablePlugins(SbtTwirl)

TwirlKeys.templateImports += "http4s.techempower.benchmark._"

com.github.retronym.SbtOneJar.oneJarSettings

val http4sVersion = "0.11.3"
val circeVersion = "0.3.0"
val doobieVersion = "0.2.3"

libraryDependencies ++= Seq(
	"org.http4s" %% "http4s-blaze-server" % http4sVersion,
	"org.http4s" %% "http4s-dsl" % http4sVersion,
	"org.http4s" %% "http4s-circe" % http4sVersion,
	"org.http4s" %% "http4s-twirl" % http4sVersion,
	"io.circe" %% "circe-core" % circeVersion,
	"io.circe" %% "circe-generic" % circeVersion,
	"io.circe" %% "circe-parser" % circeVersion,
	"org.tpolecat" %% "doobie-core" % doobieVersion,
	"org.tpolecat" %% "doobie-contrib-hikari" % doobieVersion exclude("com.zaxxer", "HikariCP-java6"),
	"com.zaxxer" %  "HikariCP" % "2.4.1",
	"org.postgresql" % "postgresql" % "9.4.1208",
	"ch.qos.logback" % "logback-classic" % "1.1.6"
)

mainClass in oneJar := Some("http4s.techempower.benchmark.WebServer")

