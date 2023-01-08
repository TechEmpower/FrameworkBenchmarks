import scala.scalanative.build._

scalaVersion := "2.13.8"

val snunitVersion = "0.2.4"
libraryDependencies ++= Seq(
  "com.github.lolgab" %%% "snunit" % snunitVersion,
  "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core" % "2.19.1",
  "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros" % "2.19.1" % "compile-internal"
)

nativeConfig ~= {
  _.withMode(Mode.releaseFull)
   .withLTO(LTO.thin)
   .withGC(GC.commix)
}

enablePlugins(ScalaNativePlugin)
