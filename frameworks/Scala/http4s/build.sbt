name := "http4s"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.7"

enablePlugins(SbtTwirl)

TwirlKeys.templateImports += "http4s.techempower.benchmark._"

com.github.retronym.SbtOneJar.oneJarSettings

val http4sVersion = "0.10.1"
val doobieVersion = "0.2.3-RC2"
val scalazVersion = "7.1.3"

libraryDependencies ++= Seq(
	"org.http4s" %% "http4s-blaze-server" % http4sVersion,
	"org.http4s" %% "http4s-dsl" % http4sVersion,
	"org.http4s" %% "http4s-argonaut" % http4sVersion,
	"org.http4s" %% "http4s-twirl" % http4sVersion,
	"org.tpolecat" %% "doobie-core" % doobieVersion,
	"org.tpolecat" %% "doobie-contrib-hikari" % doobieVersion,
	"com.zaxxer" %  "HikariCP" % "2.4.1",
	"org.scalaz" %% "scalaz-core" % scalazVersion,
	"org.scalaz" %% "scalaz-concurrent" % scalazVersion,
	"com.github.alexarchambault" %% "argonaut-shapeless_6.1" % "0.3.1",
	"org.postgresql" % "postgresql" % "9.4-1204-jdbc4"
)

mainClass in oneJar := Some("http4s.techempower.benchmark.WebServer")

resolvers ++= Seq(
  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
  "tpolecat" at "http://dl.bintray.com/tpolecat/maven"
)
