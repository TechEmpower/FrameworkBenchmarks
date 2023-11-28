name := "http4s"

version := "1.0"

scalaVersion := "2.13.11"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding",
  "UTF-8",
  "-feature",
  "-unchecked",
  "-language:reflectiveCalls",
  "-Ywarn-numeric-widen",
  "-target:11",
  "-Xlint:-byname-implicit",
  "-Xlint"
)

enablePlugins(SbtTwirl)

val http4sVersion = "0.23.22"
val http4sBlazeVersion = "0.23.15"
val http4sTwirlVersion = "0.23.17"

assembly / assemblyMergeStrategy := {
  case PathList(xs @ _*) if xs.last == "io.netty.versions.properties" => MergeStrategy.rename
  case PathList(xs @ _*) if xs.last == "module-info.class" => MergeStrategy.discard
  case other => (assembly / assemblyMergeStrategy).value(other)
}

libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-blaze-server" % http4sBlazeVersion,
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-twirl" % http4sTwirlVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,
  // Optional for auto-derivation of JSON codecs
  "io.circe" %% "circe-generic" % "0.14.5",
  "org.typelevel" %% "cats-effect" % "3.5.1",
  "co.fs2" %% "fs2-core" % "3.7.0",
  "co.fs2" %% "fs2-io" % "3.7.0",
  "io.getquill" %% "quill-jasync-postgres" % "3.19.0",
  "io.getquill" %% "quill-jasync" % "3.19.0",
  "ch.qos.logback" % "logback-classic" % "1.4.8"
)

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
