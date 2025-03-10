name := "kyo-tapir"
version := "1.0.0"
scalaVersion := "3.6.3"
lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "io.getkyo" %% "kyo-tapir" % "0.16.2",
      "com.softwaremill.sttp.tapir" %% "tapir-json-zio" % "1.11.15",
      "dev.zio" %% "zio-json" % "0.7.32"
    ),
    assembly / assemblyMergeStrategy  := {
      case x if x.contains("io.netty.versions.properties") => MergeStrategy.discard
      case x =>
        val oldStrategy = (assembly / assemblyMergeStrategy).value
        oldStrategy(x)
    }
  )
