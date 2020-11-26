name := "http4s"

version := "1.0"

scalaVersion := "2.13.3"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-unchecked",
  "-language:reflectiveCalls",
  "-Ywarn-numeric-widen",
  "-target:11",
  "-Xlint"
)

enablePlugins(SbtTwirl)

val http4sVersion = "0.21.7"

assemblyMergeStrategy in assembly := {
  case PathList(xs @ _*) if xs.last == "io.netty.versions.properties" => MergeStrategy.rename
  case other => (assemblyMergeStrategy in assembly).value(other)
}

libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-twirl" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,
  // Optional for auto-derivation of JSON codecs
  "io.circe" %% "circe-generic" % "0.13.0",
  "org.typelevel" %% "cats-effect" % "2.2.0",
  "co.fs2" %% "fs2-core" % "2.4.4",
  "co.fs2" %% "fs2-io" % "2.4.4",
  "io.getquill" %% "quill-jasync-postgres" % "3.5.2",
  "io.getquill" %% "quill-jasync" % "3.5.2",
  "ch.qos.logback" % "logback-classic" % "1.2.3"
)

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
