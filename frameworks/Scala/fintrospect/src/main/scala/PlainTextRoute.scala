import com.twitter.finagle.Service
import com.twitter.finagle.http.Method.Get
import com.twitter.finagle.http.Request
import com.twitter.io.Buf
import io.fintrospect.RouteSpec
import io.fintrospect.formats.PlainText.ResponseBuilder._

object PlainTextRoute {

  private val preallocatedMsgForPlainText = Buf.Utf8("Hello, World!")

  private val service = Service.mk { _: Request => Ok(preallocatedMsgForPlainText) }

  def apply() = RouteSpec().at(Get) / "plaintext" bindTo service
}
