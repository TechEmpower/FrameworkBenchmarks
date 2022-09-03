import scala.scalanative.build._

scalaVersion := "2.13.8"

val snunitVersion = "0.0.22"
libraryDependencies ++= Seq(
  "com.github.lolgab" %%% "snunit" % snunitVersion,
  "com.lihaoyi" %%% "upickle" % "2.0.0"
)

nativeConfig ~= {
  _.withMode(Mode.releaseFull)
   .withLTO(LTO.thin)
   .withGC(GC.commix)
}

enablePlugins(ScalaNativePlugin)
