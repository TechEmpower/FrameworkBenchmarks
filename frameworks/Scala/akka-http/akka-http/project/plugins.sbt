
// Workaround for https://github.com/coursier/coursier/issues/450
classpathTypes += "maven-plugin"

addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.8.2")
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.3.16")
addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.3")