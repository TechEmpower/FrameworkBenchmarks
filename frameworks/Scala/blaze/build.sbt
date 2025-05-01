name := "blaze"

version := "1.0"

scalaVersion := "2.13.16"

libraryDependencies ++= Seq(
	"org.http4s" %% "blaze-http" % "0.14.18",
	"com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "2.34.0",
	"ch.qos.logback" % "logback-classic" % "1.5.18"
)

crossPaths := false

assembly / assemblyMergeStrategy  := {
	case x if x.contains("module-info.class") => MergeStrategy.discard
	case x =>
		val oldStrategy = (assembly / assemblyMergeStrategy).value
		oldStrategy(x)
}