name := """techempower-benchmarks-fintrospect"""

version := "1.0"

scalaVersion := "2.12.8"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

resolvers += "JCenter" at "https://jcenter.bintray.com"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
  "io.fintrospect" %% "fintrospect-core" % "15.0.2",
  "io.fintrospect" %% "fintrospect-jackson" % "15.0.2",
  "io.fintrospect" %% "fintrospect-mustache" % "15.0.2",
  "com.twitter" %% "finagle-mysql" % "19.5.1"
)

assemblyMergeStrategy in assembly := {
 case PathList("META-INF", xs @ _*) => MergeStrategy.discard
 case x => MergeStrategy.first
}
