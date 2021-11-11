name := "blaze"

version := "1.0"

scalaVersion := "2.13.5"

libraryDependencies ++= Seq(
	"org.http4s" %% "blaze-http" % "0.14.16",
	"com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.7.3",
	"ch.qos.logback" % "logback-classic" % "1.2.3"
)

crossPaths := false