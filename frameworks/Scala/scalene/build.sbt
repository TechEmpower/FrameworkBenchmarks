val scaleneUri = uri("https://github.com/DanSimon/Scalene.git#0.1.3")

lazy val scaleneRouting = ProjectRef(scaleneUri,"scalene-routing")
lazy val scaleneSQL = ProjectRef(scaleneUri,"scalene-sql")

lazy val `scalene-benchmark` = (project in file("."))
  .dependsOn(scaleneRouting)
  .dependsOn(scaleneSQL)


assemblyMergeStrategy in assembly := {
 case PathList("META-INF", xs @ _*) => MergeStrategy.discard
 case x => MergeStrategy.first
}

scalaVersion := "2.13.1"
version := "0.1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.postgresql" % "postgresql"        % "42.2.0",
  "org.json4s"                   %% "json4s-jackson"       % "3.6.7",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.10.2"
)

