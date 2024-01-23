import org.http4k.core.Body
import org.http4k.core.ContentType.Companion.TEXT_PLAIN
import org.http4k.core.Method.GET
import org.http4k.core.Response
import org.http4k.core.Status.Companion.OK
import org.http4k.routing.bind
import java.nio.ByteBuffer.wrap
import java.nio.charset.StandardCharsets.UTF_8

object PlainTextRoute {
    private val body = Body(wrap("Hello, world!".toByteArray(UTF_8)))
    private val contentType = TEXT_PLAIN.toHeaderValue()

    operator fun invoke() = "/plaintext" bind GET to {
        Response(OK).body(body).header("Content-Type", contentType)
    }
}
