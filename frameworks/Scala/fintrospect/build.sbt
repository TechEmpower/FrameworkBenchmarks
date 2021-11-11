name := """techempower-benchmarks-fintrospect"""

version := "1.0"

scalaVersion := "2.13.3"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

resolvers += "JCenter" at "https://jcenter.bintray.com"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++= Seq(
  "io.fintrospect" %% "fintrospect-core" % "17.0.0",
  "io.fintrospect" %% "fintrospect-jackson" % "17.0.0",
  "io.fintrospect" %% "fintrospect-mustache" % "17.0.0",
  "com.twitter" %% "finagle-mysql" % "20.8.0"
)

assemblyMergeStrategy in assembly := {
 case PathList("META-INF", xs @ _*) => MergeStrategy.discard
 case x => MergeStrategy.first
}
