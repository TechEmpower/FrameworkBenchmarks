import scala.scalanative.build._

scalaVersion := "2.13.7"

val snunitVersion = "0.0.11"
libraryDependencies ++= Seq(
  "com.github.lolgab" %%% "snunit" % snunitVersion,
  "com.github.lolgab" %%% "snunit-async" % snunitVersion,
  "com.lihaoyi" %%% "upickle" % "1.4.2"
)

nativeConfig ~= {
  _.withMode(Mode.releaseFull)
   .withLTO(LTO.thin)
}

enablePlugins(ScalaNativePlugin)
