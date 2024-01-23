name := "play2-scala-slick"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala, PlayNettyServer).disablePlugins(PlayFilters)

scalaVersion := "2.13.12"

libraryDependencies ++= Seq(
  guice,
  "org.playframework" %% "play-slick" % "6.0.0",
  "com.mysql" % "mysql-connector-j" % "8.3.0",
  filters
)
