import sbt._
import Keys._
import com.typesafe.sbt.SbtNativePackager._

object Bench extends Build {

  lazy val project = Project(
    "bench", 
    file("."),
    settings = sharedSettings
  )

  val sharedSettings = Project.defaultSettings ++ packageArchetype.java_application ++ Seq(
    scalaVersion := "2.10.3",
    version := "1.0.0",
    libraryDependencies ++= Seq(
      "org.cognition" % "cognition" % "0.9.0-ALPHA11",
      "mysql" % "mysql-connector-java" % "5.1.28",
      "com.jolbox" % "bonecp" % "0.8.0.RELEASE",
      "fi.reaktor" %% "sqltyped" % "0.3.2",
      "com.scalatags" % "scalatags_2.10" % "0.2.2"
    ),
    resolvers := Seq(
      "cognition" at "https://raw.github.com/pakunoda/mvn-repo/master",
      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
    ),
    initialize ~= { _ => initSqltyped }
  )

  def initSqltyped {
    System.setProperty("sqltyped.url", "jdbc:mysql://localhost:3306/hello_world")
    System.setProperty("sqltyped.driver", "com.mysql.jdbc.Driver")
    System.setProperty("sqltyped.username", "benchmarkdbuser")
    System.setProperty("sqltyped.password", "benchmarkdbpass")
    System.setProperty("sqltyped.schema", "hello_world")
  }

}
