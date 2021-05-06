import scala.scalanative.build._

scalaVersion := "2.13.4"

val snunitVersion = "0.0.9"
libraryDependencies ++= Seq(
  "com.github.lolgab" %%% "snunit" % snunitVersion,
  "com.github.lolgab" %%% "snunit-async" % snunitVersion,
  "com.lihaoyi" %%% "upickle" % "1.2.3"
)

nativeConfig ~= {
  _.withMode(Mode.releaseFull)
   .withLTO(LTO.thin)
}

enablePlugins(ScalaNativePlugin)
