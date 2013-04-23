package hello

import org.scalatra.json.JacksonJsonSupport
import org.json4s.{DefaultFormats, Formats}
import org.scalatra.ScalatraServlet

/**
 * Created with IntelliJ IDEA.
 * User: mmazzarolo
 * Date: 4/10/13
 * Time: 6:57 PM
 * To change this template use File | Settings | File Templates.
 */
trait JsonSetup extends JacksonJsonSupport {

  that: ScalatraServlet =>

  protected implicit val jsonFormats: Formats = DefaultFormats

  before() {
    contentType = formats("json")
  }
}
