name := "kyo-scheduler-benchmark"

ThisBuild / version := "1.0.0"

val kyoVersion = "0.17.0"

val commonAssemblySettings = assembly / assemblyMergeStrategy := {
  case x if x.contains("io.netty.versions.properties") => MergeStrategy.discard
  case x if x.contains("module-info.class") => MergeStrategy.discard
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}

// based on the framework/Scala/zio-http implementation
lazy val `zio-http` = (project in file("zio-http"))
  .settings(
    scalaVersion := "3.6.4",
    name := "zio-http-kyo-scheduler-benchmark",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-http" % "3.2.0",
      "io.getkyo" %% "kyo-scheduler-zio" % kyoVersion,
    ),
    commonAssemblySettings
  )

val http4sVersion = "0.23.22"
val http4sBlazeVersion = "0.23.15"
val http4sTwirlVersion = "0.23.17"

// based on the framework/Scala/http4s implementation
lazy val http4s = (project in file("http4s"))
  .settings(
    scalaVersion := "2.13.16",
    name := "http4s-kyo-scheduler-benchmark",
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-blaze-server" % http4sBlazeVersion,
      "org.http4s" %% "http4s-dsl" % http4sVersion,
      "org.http4s" %% "http4s-twirl" % http4sTwirlVersion,
      "org.http4s" %% "http4s-circe" % http4sVersion,
      // Optional for auto-derivation of JSON codecs
      "io.circe" %% "circe-generic" % "0.14.5",
      "org.typelevel" %% "cats-effect" % "3.5.1",
      "co.fs2" %% "fs2-core" % "3.7.0",
      "co.fs2" %% "fs2-io" % "3.7.0",
      "io.getquill" %% "quill-jasync-postgres" % "3.19.0",
      "io.getquill" %% "quill-jasync" % "3.19.0",
      "ch.qos.logback" % "logback-classic" % "1.4.8",
      "io.getkyo" %% "kyo-scheduler-cats" % kyoVersion,
    ),
    commonAssemblySettings,
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
  )
  .enablePlugins(SbtTwirl)