package code

import org.rogach.scallop._

class Options(args: Seq[String]) extends ScallopConf(args) {
  version("lift-stateless, %s b%s (%3$td.%3$tm.%3$tY %3$tH:%3$tM). Built with Scala %4$s" format (
    BuildInfo.version,
    BuildInfo.buildinfoBuildnumber,
    new java.util.Date(BuildInfo.buildTime),
    BuildInfo.scalaVersion))

  val properties = props[String]('C')

}
