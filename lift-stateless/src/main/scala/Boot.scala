package bootstrap.liftweb

import net.liftweb._
import util._
import Helpers._

import common._
import http._
import auth._
import sitemap._
import Loc._

import code._

class Boot {
  def boot() {
    LiftRules.addToPackages("code")
    LiftRules.htmlProperties.default.set((r: Req) => new Html5Properties(r.userAgent))
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))


    LiftRules.setSiteMap(SiteMap())

    lib.JsonBenchmark.init()
    lib.DbBenchmark.init()

    // wrap the request in db
/*    S.addAround( new LoanWrapper {
      def apply[T](f: => T): T = {
        code.db.DB.exec {
          f
        }
      }
    })
*/
    sys.addShutdownHook {
      code.Main.server.stop() // jetty server isn't smart enough to stop itself on sigterm
    }
    LiftRules.unloadHooks.append({ () =>
      // lift unload, happens after all remaining requests were served
      println("Lift server stopped. (x_x)")
    })

  }
}
