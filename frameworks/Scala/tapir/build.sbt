name := "tapir-benchmark"

ThisBuild / version := "1.0.0"
ThisBuild / scalaVersion := "3.6.4"

val tapirVersion = "1.11.24"

val commonAssemblySettings = assembly / assemblyMergeStrategy := {
  case x if x.contains("io.netty.versions.properties") => MergeStrategy.discard
  case x if x.contains("module-info.class") => MergeStrategy.discard
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}

lazy val common = (project in file("common"))
  .settings(
    name := "tapir-benchmark-common"
  )

lazy val `zio-http-server` = (project in file("zio-http-server"))
  .dependsOn(common)
  .settings(
    name := "tapir-zio-http-server",
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.tapir" %% "tapir-zio-http-server" % tapirVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-json-zio" % tapirVersion
    ),
    commonAssemblySettings
  )

lazy val `http4s-server` = (project in file("http4s-server"))
  .dependsOn(common)
  .settings(
    name := "tapir-http4s-server",
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.tapir" %% "tapir-http4s-server" % tapirVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % tapirVersion,
      "org.http4s" %% "http4s-blaze-server" % "0.23.17",
    ),
    commonAssemblySettings
  )

lazy val `http4s-server-zio` = (project in file("http4s-server-zio"))
  .dependsOn(common)
  .settings(
    name := "tapir-http4s-server-zio",
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.tapir" %% "tapir-http4s-server-zio" % tapirVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % tapirVersion,
      "org.http4s" %% "http4s-blaze-server" % "0.23.17",
      "dev.zio" %% "zio-interop-cats" % "23.1.0.5"
    ),
    commonAssemblySettings
  )

lazy val `netty-zio-server` = (project in file("netty-zio-server"))
  .dependsOn(common)
  .settings(
    name := "tapir-netty-zio-server",
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.tapir" %% "tapir-netty-server-zio" % tapirVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-json-zio" % tapirVersion,
    ),
    commonAssemblySettings
  )

lazy val `netty-cats-server` = (project in file("netty-cats-server"))
  .dependsOn(common)
  .settings(
    name := "tapir-netty-cats-server",
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.tapir" %% "tapir-netty-server-cats" % tapirVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % tapirVersion,
      "org.http4s" %% "http4s-blaze-server" % "0.23.17",
    ),
    commonAssemblySettings
  )

lazy val `pekko-http-server` = (project in file("pekko-http-server"))
  .dependsOn(common)
  .settings(
    name := "tapir-pekko-http-server",
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.tapir" %% "tapir-pekko-http-server" % tapirVersion,
      "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % tapirVersion,
    ),
    commonAssemblySettings
  )
