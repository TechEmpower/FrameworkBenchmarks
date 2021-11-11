import org.http4k.core.Body
import org.http4k.core.Method.GET
import org.http4k.core.Response
import org.http4k.core.Status.Companion.OK
import org.http4k.core.with
import org.http4k.format.Jackson.json
import org.http4k.format.Jackson.obj
import org.http4k.format.Jackson.string
import org.http4k.routing.bind

object JsonRoute {
    private val jsonBody = Body.json().toLens()

    operator fun invoke() = "/json" bind GET to { Response(OK).with(jsonBody of obj("message" to string("Hello, World!"))) }
}