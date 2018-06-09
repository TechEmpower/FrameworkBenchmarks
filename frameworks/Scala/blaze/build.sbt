name := "blaze"

version := "1.0"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(
	"org.http4s" %% "blaze-http" % "0.14.0-M3",
	"com.github.plokhotnyuk.jsoniter-scala" %% "macros" % "0.27.1"
)
