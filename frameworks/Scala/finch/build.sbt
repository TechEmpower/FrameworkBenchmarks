name := """techempower-benchmarks-finch"""

version := "1.0"

scalaVersion := "2.12.5"

libraryDependencies ++= Seq(
  "com.github.finagle" %% "finch-core" % "0.18.0",
  "com.github.finagle" %% "finch-circe" % "0.18.0"
)

assemblyMergeStrategy in assembly := {
 case PathList("META-INF", xs @ _*) => MergeStrategy.discard
 case x => MergeStrategy.first
}
