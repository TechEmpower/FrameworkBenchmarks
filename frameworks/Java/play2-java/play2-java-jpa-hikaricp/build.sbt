name := "play2-java-jpa-hikaricp"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayMinimalJava, PlayNettyServer).disablePlugins(PlayFilters)

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  guice,
  javaJpa,
  "mysql" % "mysql-connector-java" % "8.0.18",
  "org.hibernate" % "hibernate-core" % "5.4.1.Final"
)

PlayKeys.externalizeResourcesExcludes += baseDirectory.value / "conf" / "META-INF" / "persistence.xml"
