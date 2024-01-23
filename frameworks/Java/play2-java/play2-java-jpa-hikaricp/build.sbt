name := "play2-java-jpa-hikaricp"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayMinimalJava, PlayNettyServer).disablePlugins(PlayFilters)

scalaVersion := "2.13.12"

libraryDependencies ++= Seq(
  guice,
  javaJpa,
  "com.mysql" % "mysql-connector-j" % "8.3.0",
  "org.hibernate" % "hibernate-core" % "6.4.2.Final"
)

PlayKeys.externalizeResourcesExcludes += baseDirectory.value / "conf" / "META-INF" / "persistence.xml"
