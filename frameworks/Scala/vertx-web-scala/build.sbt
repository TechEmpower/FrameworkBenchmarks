name := "vertx-web-scala"

version := "1"

scalaVersion := "3.7.4"

lazy val root = (project in file(".")).enablePlugins(SbtTwirl)

val vertxScalaVersion = "5.0.0.CR2"
val vertxVersion = "5.0.5"
val nettyVersion = "4.2.7.Final" // the version that Vert.x depends on

libraryDependencies += "io.vertx" %% "vertx-lang-scala" % vertxScalaVersion
libraryDependencies += "io.vertx" % "vertx-web" % vertxVersion
libraryDependencies += "io.vertx" % "vertx-codegen" % vertxVersion
libraryDependencies += "io.vertx" % "vertx-pg-client" % vertxVersion
libraryDependencies += "io.netty" % "netty-transport-native-kqueue" % nettyVersion classifier "osx-x86_64"
//libraryDependencies += "io.netty" % "netty-transport-native-epoll" % nettyVersion classifier "linux-x86_64"
libraryDependencies += "io.netty" % "netty-transport-native-io_uring" % nettyVersion classifier "linux-x86_64"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.5.22"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.6"

Compile / mainClass := Some("vertx.App")

assembly / assemblyMergeStrategy := {
  case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
  case _                                   => MergeStrategy.first
}
