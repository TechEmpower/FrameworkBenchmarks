addCommandAlias("cpl", "all compile test:compile")
addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")

val http4sVersion = "0.18.2"
val circeVersion = "0.9.2"
val doobieVersion = "0.5.1"
val catsEffectVersion = "0.10"
val catsVersion = "1.0.1"

lazy val root = (project in file("."))
  .settings(commonSettings)
  .settings(baseAssemblySettings)
  .settings(
    mainClass in (Compile, run) := Some("http4s.techempower.benchmark.Main"))
  .settings(mainClass in assembly := Some("http4s.techempower.benchmark.Main"))
  .settings(assemblyJarName in assembly := "http4s-benchmark.jar")
  .settings(libraryDependencies ++= List(
    "org.typelevel" %% "cats-effect" % catsEffectVersion,
    "org.typelevel" %% "cats-core" % "0.9.0",
    "org.http4s" %% "http4s-blaze-server" % http4sVersion,
    "org.http4s" %% "http4s-blaze-client" % http4sVersion,
    "org.http4s" %% "http4s-dsl" % http4sVersion,
    "org.http4s" %% "http4s-circe" % http4sVersion,
    "org.http4s" %% "http4s-twirl" % http4sVersion,
    "io.circe" %% "circe-core" % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion,
    "io.circe" %% "circe-parser" % circeVersion,
    "io.circe" %% "circe-literal" % circeVersion,
    "org.tpolecat" %% "doobie-core" % doobieVersion,
    "org.postgresql" % "postgresql" % "42.1.4",
    "com.github.pureconfig" %% "pureconfig" % "0.9.0"
  ))

lazy val commonSettings = List(
  name := "http4s",
  version := "1.0-SNAPSHOT",
  scalaVersion := "2.12.4",
  scalacOptions ++= List(
    "-Ypartial-unification",
    "-Xexperimental"
  )
)
