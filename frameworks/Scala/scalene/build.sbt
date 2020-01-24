val scaleneUri = uri("https://github.com/DanSimon/Scalene.git#0.1.0")

lazy val scaleneRouting = ProjectRef(scaleneUri,"scalene-routing")
lazy val scaleneSQL = ProjectRef(scaleneUri,"scalene-sql")

lazy val `scalene-benchmark` = (project in file("."))
  .dependsOn(scaleneRouting)
  .dependsOn(scaleneSQL)

scalaVersion := "2.12.10"
version := "0.1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  "org.postgresql" % "postgresql"        % "42.2.0",
  "org.json4s"                   %% "json4s-jackson"       % "3.5.3",
  "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.9.2"
)

