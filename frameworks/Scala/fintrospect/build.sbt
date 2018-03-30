name := """techempower-benchmarks-fintrospect"""

version := "1.0"

scalaVersion := "2.12.5"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

resolvers += "JCenter" at "https://jcenter.bintray.com"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
  "io.fintrospect" %% "fintrospect-core" % "14.15.0",
  "io.fintrospect" %% "fintrospect-jackson" % "14.15.0",
  "io.fintrospect" %% "fintrospect-mustache" % "14.15.0",
  "com.twitter" %% "finagle-mysql" % "6.43.0"
)

assemblyMergeStrategy in assembly := {
 case PathList("META-INF", xs @ _*) => MergeStrategy.discard
 case x => MergeStrategy.first
}
