
// Workaround for https://github.com/coursier/coursier/issues/450
classpathTypes += "maven-plugin"

addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.8.1")
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.2.2")
addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-RC11")

