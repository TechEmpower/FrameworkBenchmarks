name := "http4s"

version := "1.0-SNAPSHOT"

scalaVersion := "2.12.4"

//TwirlKeys.templateImports += "http4s.techempower.benchmark._"
//
//com.github.retronym.SbtOneJar.oneJarSettings

val http4sVersion = "0.18.2"
val circeVersion = "0.9.2"
val doobieVersion = "0.5.1"
val catsEffectVersion = "0.10"
// NOTE: Bump to 1.0.1 when cats effect 1.0.0 is release 04/2017
val catsVersion = "1.0.1"

libraryDependencies ++= List(
	"org.typelevel" %% "cats-effect" % catsEffectVersion,
	"org.typelevel" %% "cats-core" % "0.9.0",
	"org.http4s" %% "http4s-blaze-server" % http4sVersion,
	"org.http4s" %% "http4s-blaze-client" % http4sVersion,
	"org.http4s" %% "http4s-dsl" % http4sVersion,
	"org.http4s" %% "http4s-circe" % http4sVersion,
	"org.http4s" %% "http4s-twirl" % http4sVersion,
	"io.circe" %% "circe-core" % circeVersion,
	"io.circe" %% "circe-generic" % circeVersion,
	"io.circe" %% "circe-parser" % circeVersion,
	"io.circe" %% "circe-literal" % circeVersion,
	"org.tpolecat" %% "doobie-core" % doobieVersion,
	"org.tpolecat" %% "doobie-hikari" % doobieVersion,
	"com.zaxxer" %  "HikariCP" % "2.6.1",
	"org.postgresql" % "postgresql" % "42.1.4",
	"ch.qos.logback" % "logback-classic" % "1.2.3",
	"com.github.pureconfig" %% "pureconfig" % "0.9.0"
)

//mainClass in oneJar := Some("http4s.techempower.benchmark.WebServer")

scalacOptions ++= List(
	"-Ypartial-unification",
	"-Xexperimental"
)

