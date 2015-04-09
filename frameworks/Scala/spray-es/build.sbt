organization  := "fi.markoa"

version       := "0.1"

scalaVersion  := "2.11.6"

scalacOptions := Seq("-encoding", "utf8")

libraryDependencies ++= {
  val akkaV = "2.3.9"
  val sprayV = "1.3.3"
  val json4sV = "3.2.11"
  Seq(
    "io.spray" %% "spray-can" % sprayV,
    "io.spray" %% "spray-routing" % sprayV,
    "io.spray" %% "spray-client" % sprayV,
    "org.json4s" %% "json4s-jackson" % json4sV,
    "org.json4s" %% "json4s-ext" % json4sV,
    "com.typesafe.akka" %%  "akka-actor" % akkaV,
    "com.typesafe" % "config" % "1.2.1",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
    "ch.qos.logback" % "logback-classic" % "1.1.3",
    "io.spray" %% "spray-testkit" % sprayV % "test",
    "com.typesafe.akka" %%  "akka-testkit" % akkaV % "test",
    "org.specs2" %% "specs2-core" % "2.3.11" % "test"
  )
}

Revolver.settings
