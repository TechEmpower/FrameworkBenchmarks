import scala.scalanative.build._

scalaVersion := "2.13.10"

val snunitVersion = "0.3.0"
val jsoniterScalaVersion = "2.20.6"

libraryDependencies ++= Seq(
  "com.github.lolgab" %%% "snunit" % snunitVersion,
  "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core" % jsoniterScalaVersion,
  "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros" % jsoniterScalaVersion % "compile-internal"
)

nativeConfig ~= {
  _.withMode(Mode.releaseFull)
   .withLTO(LTO.thin)
   .withGC(GC.commix)
}

enablePlugins(ScalaNativePlugin)
