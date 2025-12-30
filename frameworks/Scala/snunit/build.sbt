import scala.scalanative.build._

scalaVersion := "3.5.2"

val snunitVersion = "0.10.3"
val jsoniterScalaVersion = "2.33.0"

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
