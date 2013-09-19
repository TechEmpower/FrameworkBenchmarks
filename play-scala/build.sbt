name := "play-scala"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(jdbc, anorm, "mysql" % "mysql-connector-java" % "5.1.22")

dependencyOverrides += "com.jolbox" % "bonecp" % "0.7.1.RELEASE"

playScalaSettings
