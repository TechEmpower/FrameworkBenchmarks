import scala.scalanative.build._

scalaVersion := "2.13.8"

val snunitVersion = "0.0.17"
libraryDependencies ++= Seq(
  "com.github.lolgab" %%% "snunit" % snunitVersion,
  "com.lihaoyi" %%% "upickle" % "1.6.0"
)

nativeConfig ~= {
  _.withMode(Mode.releaseFull)
   .withLTO(LTO.thin)
}

enablePlugins(ScalaNativePlugin)
