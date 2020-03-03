name := "blaze"

version := "1.0"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
	"org.http4s" %% "http4s-blaze-core" % "0.20.3",
	"com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "0.51.3",
	"ch.qos.logback" % "logback-classic" % "1.2.3"
)
