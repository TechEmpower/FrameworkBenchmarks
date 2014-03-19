name := "play-java-ebean"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  javaJdbc,
  javaEbean,
  "mysql" % "mysql-connector-java" % "5.1.22"
  )

dependencyOverrides += "com.jolbox" % "bonecp" % "0.7.1.RELEASE"

playJavaSettings
