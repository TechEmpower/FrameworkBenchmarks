name := "play-java-jpa"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  javaJdbc,
  javaJpa,
  "mysql" % "mysql-connector-java" % "5.1.22",
  "org.hibernate" % "hibernate-entitymanager" % "4.2.1.Final"
  )

playJavaSettings
