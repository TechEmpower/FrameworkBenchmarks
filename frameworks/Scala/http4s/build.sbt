name := "http4s"

version := "1.0"

scalaVersion := "2.12.6"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",
  "-language:reflectiveCalls",
  "-Yno-adapted-args",
  "-Ypartial-unification",
  "-Ywarn-numeric-widen",
  "-Xfuture",
  "-Xlint"
)

enablePlugins(SbtTwirl)

TwirlKeys.templateImports += "http4s.techempower.benchmark._"

val http4sVersion = "0.18.12"
val circeVersion = "0.9.3"
val doobieVersion = "0.5.3"

libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-twirl" % http4sVersion,
  "com.github.plokhotnyuk.jsoniter-scala" %% "macros" % "0.21.6",
  "org.tpolecat" %% "doobie-core" % doobieVersion,
  "org.tpolecat" %% "doobie-hikari" % doobieVersion,
  "org.postgresql" % "postgresql" % "42.2.2",
  "ch.qos.logback" % "logback-classic" % "1.2.3"
)
