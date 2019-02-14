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
        library.akkaSlf4j,
        library.akkaStream,
        library.akkaHttp,
        library.akkaHttpCirce,
        library.akkaTestkit        % Test,
        library.akkaHttpTestkit    % Test,
        library.akkaStreamTestkit  % Test,
        library.logbackClassic,
        library.postgresql,
        library.scalaCheck         % Test,
        library.scalaTest          % Test,
        library.scalateCore,
        library.slick,
        library.slickPgCore,
        library.slickHikariCP,
      ),
      parallelExecution in Test := true
    )

lazy val library =
  new {
    object Version {
      val akka              = "2.5.20"
      val akkaHttp          = "10.1.7"
      val akkaHttpCirce     = "1.23.0"
      val logback           = "1.2.3"
      val postgresql        = "42.2.5"
      val scalaCheck        = "1.14.0"
      val scalaTest         = "3.0.5"
      val scalate           = "1.8.0"
      val slick             = "3.3.0"
      val slickPg           = "0.17.2"
    }

    val akkaSlf4j           = "com.typesafe.akka"            %% "akka-slf4j"                  % Version.akka
    val akkaHttp            = "com.typesafe.akka"            %% "akka-http"                   % Version.akkaHttp
    val akkaHttpTestkit     = "com.typesafe.akka"            %% "akka-http-testkit"           % Version.akkaHttp
    val akkaHttpCirce       = "de.heikoseeberger"            %% "akka-http-circe"             % Version.akkaHttpCirce
    val akkaStream          = "com.typesafe.akka"            %% "akka-stream"                 % Version.akka
    val akkaStreamTestkit   = "com.typesafe.akka"            %% "akka-stream-testkit"         % Version.akka
    val akkaTestkit         = "com.typesafe.akka"            %% "akka-testkit"                % Version.akka
    val logbackClassic      = "ch.qos.logback"               %  "logback-classic"             % Version.logback
    val postgresql          = "org.postgresql"               %  "postgresql"                  % Version.postgresql
    val scalaCheck          = "org.scalacheck"               %% "scalacheck"                  % Version.scalaCheck
    val scalaTest           = "org.scalatest"                %% "scalatest"                   % Version.scalaTest
    val scalateCore         = "org.scalatra.scalate"         %% "scalate-core"                % Version.scalate
    val slick               = "com.typesafe.slick"           %% "slick"                       % Version.slick
    val slickHikariCP       = "com.typesafe.slick"           %% "slick-hikaricp"              % Version.slick
    val slickPgCore         = "com.github.tminglei"          %% "slick-pg"                    % Version.slickPg
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
    scalaVersion := "2.12.6",
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding",
      "UTF-8",
      "-explaintypes",
      "-feature",
      "-target:jvm-1.8",
      "-unchecked",
      "-Xfatal-warnings",
      "-Xfuture",
      "-Xlint",
      "-Ydelambdafy:method",
      "-Yno-adapted-args",
      "-Ypartial-unification",
      "-Ywarn-numeric-widen",
      "-Ywarn-unused-import",
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
    javaOptions ++= Seq(
      "-jvm-debug 5555"
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
