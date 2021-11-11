name := "vertx-web-scala"

version := "1"

scalaVersion := "2.12.8"

lazy val root = (project in file(".")).enablePlugins(SbtTwirl)

libraryDependencies += "io.vertx" %% "vertx-lang-scala" % "3.7.1"
libraryDependencies += "io.vertx" %% "vertx-web-scala" % "3.7.1"
libraryDependencies += "io.vertx" % "vertx-codegen" % "3.7.1"
libraryDependencies += "io.netty" % "netty-transport-native-kqueue" % "4.1.36.Final" classifier "osx-x86_64"
libraryDependencies += "io.netty" % "netty-transport-native-epoll" % "4.1.36.Final" classifier "linux-x86_64"
libraryDependencies += "io.reactiverse" % "reactive-pg-client" % "0.11.3"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"

mainClass in Compile := Some("vertx.App")

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
  case _ => MergeStrategy.first
}