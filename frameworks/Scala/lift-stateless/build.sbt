import AssemblyKeys._

organization := "code"

name := "lift-stateless"

version := "0.0.1"

scalaVersion := "2.10.1"

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked", 
  "-feature", 
  "-language:postfixOps",
  "-language:reflectiveCalls",
  "-language:implicitConversions",
  "-Xlint")

libraryDependencies ++= Seq(
  "org.streum" %% "configrity-core" % "1.0.0",
  "org.rogach" %% "scallop" % "0.8.1",
  "commons-dbcp" % "commons-dbcp" % "1.4",
  "mysql" % "mysql-connector-java" % "5.1.21",
  "com.typesafe.slick" %% "slick" % "1.0.0",
  "ch.qos.logback" % "logback-classic" % "1.0.9",
  "org.eintr.loglady" %% "loglady" % "1.1.0",
  "org.eclipse.jetty" % "jetty-server" % "8.1.9.v20130131",
  "org.eclipse.jetty" % "jetty-webapp" % "8.1.9.v20130131",
  "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" artifacts Artifact("javax.servlet", "jar", "jar"),
  "net.liftweb" %% "lift-webkit" % "2.5-RC2"
)

assemblySettings

mainClass in assembly := Some("code.Main")

mergeStrategy in assembly <<= (mergeStrategy in assembly) { old =>
  {
    case "index.html" => MergeStrategy.first
    case "about.html" => MergeStrategy.discard
    case x => old(x)
  }
}

seq(Revolver.settings: _*)

mainClass in Revolver.reStart := Some("code.Main")

buildInfoSettings

sourceGenerators in Compile <+= buildInfo

buildInfoKeys ++= Seq[BuildInfoKey](
  name,
  version,
  scalaVersion,
  sbtVersion,
  buildInfoBuildNumber,
  BuildInfoKey.action("buildTime") { System.currentTimeMillis }
)

buildInfoPackage := "code"
