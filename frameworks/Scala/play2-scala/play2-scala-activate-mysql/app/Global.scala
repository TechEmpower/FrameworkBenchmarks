import play.api._
import com.typesafe.config.ConfigFactory

object Global extends GlobalSettings {
	System.setProperty("activate.offlineLocking.enable", "true")
	System.setProperty("activate.offlineLocking.validateReads", "true")
}