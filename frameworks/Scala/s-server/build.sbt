name := "s-server-tfb"

organization := "woshilaiceshide"

scalaVersion := "2.11.8"

version := "1.0"

scalacOptions := Seq("-unchecked", "-deprecation","-optimise", "-encoding", "utf8", "-Yno-adapted-args", "-target:jvm-1.8")

javacOptions ++= Seq("-Xlint:deprecation", "-Xlint:unchecked", "-source", "1.8", "-target", "1.8")

enablePlugins(JavaAppPackaging)

resolvers += "Woshilaiceshide Releases" at "http://dl.bintray.com/woshilaiceshide/maven/"

libraryDependencies += "woshilaiceshide" %% "s-server" % "2.2"

//libraryDependencies += "com.fasterxml.jackson.core" % "jackson-core" % "2.7.4"

//libraryDependencies += "com.fasterxml.jackson.module" % "jackson-module-afterburner" % "2.7.4"

libraryDependencies += "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.7.4"
