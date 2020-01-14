name := "http4s"

version := "1.0"

scalaVersion := "2.12.8"

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

val http4sVersion = "0.20.3"
val doobieVersion = "0.7.0"

libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-twirl" % http4sVersion,
  "com.github.plokhotnyuk.jsoniter-scala" %% "jsoniter-scala-macros" % "0.51.3",
  "org.tpolecat" %% "doobie-core" % doobieVersion,
  "org.tpolecat" %% "doobie-hikari" % doobieVersion,
  "org.postgresql" % "postgresql" % "42.2.6",
  "ch.qos.logback" % "logback-classic" % "1.2.3"
)
