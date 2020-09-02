lazy val akkaHttpSlickPostgres =
  project
    .in(file("."))
    .enablePlugins(
      GitBranchPrompt,
      GitVersioning,
      JavaAppPackaging,
      DockerPlugin,
      AshScriptPlugin
    )
    .settings(settings)
    .settings(
      name := "akka-http-slick-postgres",
      mainClass in Compile := Some("net.benchmark.akka.http.Main"),
      libraryDependencies ++= Seq(
        library.akkaHttp,
        library.akkaHttpCirce,
        library.circeGeneric,
        library.akkaSlf4j,
        library.akkaStream,
        library.logbackClassic,
        library.postgresql,
        library.scalateCore,
        library.slick,
        library.slickHikariCP,
      ),
      parallelExecution in Test := true
    )

lazy val library =
  new {
    object Version {
      val akka              = "2.6.8"
      val akkaHttp          = "10.2.0"
      val akkaHttpCirce     = "1.34.0"
      val circe             = "0.13.0"
      val logback           = "1.2.3"
      val postgresql        = "42.2.16"
      val scalate           = "1.9.6"
      val slick             = "3.3.2"
    }

    val akkaHttp            = "com.typesafe.akka"            %% "akka-http"                   % Version.akkaHttp
    val akkaHttpCirce       = "de.heikoseeberger"            %% "akka-http-circe"             % Version.akkaHttpCirce
    val circeGeneric        = "io.circe"                     %% "circe-generic"               % Version.circe
    val akkaSlf4j           = "com.typesafe.akka"            %% "akka-slf4j"                  % Version.akka
    val akkaStream          = "com.typesafe.akka"            %% "akka-stream"                 % Version.akka
    val logbackClassic      = "ch.qos.logback"               %  "logback-classic"             % Version.logback
    val postgresql          = "org.postgresql"               %  "postgresql"                  % Version.postgresql
    val scalateCore         = "org.scalatra.scalate"         %% "scalate-core"                % Version.scalate
    val slick               = "com.typesafe.slick"           %% "slick"                       % Version.slick
    val slickHikariCP       = "com.typesafe.slick"           %% "slick-hikaricp"              % Version.slick
  }

lazy val settings =
  commonSettings ++
    gitSettings ++
    packagingSettings ++
    scalafmtSettings

lazy val commonSettings =
  Seq(
    organization := "net.benchmark.akka.http",
    organizationName := "Akka",
    scalaVersion := "2.13.3",
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding",
      "UTF-8",
      "-explaintypes",
      "-feature",
      "-target:jvm-1.8",
      "-unchecked",
      "-Xfatal-warnings",
      "-Xlint:unused",
      "-Ydelambdafy:method",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard"
    ),
    scalacOptions in (Compile, console) --= Seq("-Xfatal-warnings"), // Relax settings for console
    scalacOptions in (Test, console) --= Seq("-Xfatal-warnings"), // Relax settings for console
    javacOptions ++= Seq(
      "-encoding",
      "UTF-8",
      "-source",
      "1.8",
      "-target",
      "1.8"
    ),
    transitiveClassifiers := Seq("sources"),
    publishArtifact in (Compile, packageDoc) := false,
    unmanagedSourceDirectories.in(Compile) := Seq(scalaSource.in(Compile).value),
    unmanagedSourceDirectories.in(Test) := Seq(scalaSource.in(Test).value),
    wartremoverWarnings in (Compile, compile) ++= Warts.unsafe
  )

lazy val gitSettings =
  Seq(
    git.useGitDescribe := true
  )

lazy val packagingSettings =
  Seq(
    mappings in Universal += {
      // we are using the reference.conf as default application.conf
      // the user can override settings here
      val conf = (resourceDirectory in Compile).value / "reference.conf"
      conf -> "conf/application.conf"
    },
    scriptClasspath := Seq("../conf/") ++ scriptClasspath.value,
    daemonUser.in(Docker) := "root",
    maintainer.in(Docker) := "sourcekick",
    version.in(Docker) := version.value,
    dockerBaseImage := "openjdk:jre-alpine",
    dockerExposedPorts := Seq(8080),
    dockerExposedVolumes in Docker := Seq("/config"),
    mappings in Universal += {
      var appjar = (packageBin in Test).value
      appjar -> s"lib/${appjar.getName}"
    }
  )

lazy val scalafmtSettings =
  Seq(
    scalafmtOnCompile := true,
    scalafmtOnCompile.in(Sbt) := false,
    scalafmtVersion := "1.5.1"
  )
