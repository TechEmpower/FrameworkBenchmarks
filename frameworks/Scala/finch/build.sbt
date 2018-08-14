lazy val finchVersion = "0.23.0"

name := """techempower-benchmarks-finch"""
version := finchVersion
scalaVersion := "2.12.5"

libraryDependencies ++= Seq(
  "com.github.finagle" %% "finch-arrows-core" % finchVersion,
  "com.github.finagle" %% "finch-arrows-circe" % finchVersion
)

assemblyJarName in assembly := "finch-benchmark.jar"
assemblyMergeStrategy in assembly := {
 case PathList("META-INF", "services", _*) => MergeStrategy.last
 case PathList("META-INF", _*) => MergeStrategy.discard
 case _ => MergeStrategy.first
}
