name := "vertx-web-scala"

version := "1"

scalaVersion := "2.12.17"

lazy val root = (project in file(".")).enablePlugins(SbtTwirl)

val vertxScalaVersion = "3.9.1"
val vertxVersion = "3.9.15"
val nettyVersion = "4.1.89.Final"

libraryDependencies += "io.vertx" %% "vertx-lang-scala" % vertxScalaVersion
libraryDependencies += "io.vertx" %% "vertx-web-scala" % vertxScalaVersion
libraryDependencies += "io.vertx" % "vertx-codegen" % vertxVersion
libraryDependencies += "io.vertx" % "vertx-pg-client" % vertxVersion
libraryDependencies += "io.netty" % "netty-transport-native-kqueue" % nettyVersion classifier "osx-x86_64"
libraryDependencies += "io.netty" % "netty-transport-native-epoll" % nettyVersion classifier "linux-x86_64"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.4.5"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"

Compile / mainClass := Some("vertx.App")

assembly / assemblyMergeStrategy := {
  case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
  case _                                   => MergeStrategy.first
}
