package conf

import org.cognition._

object Authentication extends Security[String] {

  def token()(implicit request: Request) = None

}
