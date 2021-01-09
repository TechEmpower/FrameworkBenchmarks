scalaVersion := "2.11.12"

libraryDependencies ++= Seq(
  "com.github.lolgab" %%% "snunit" % "0.0.2",
  "com.github.lolgab" %%% "snunit-async" % "0.0.2",
  "com.lihaoyi" %%% "upickle" % "1.2.2"
)

nativeMode := "release-full"
nativeLTO := "thin"

enablePlugins(ScalaNativePlugin)
