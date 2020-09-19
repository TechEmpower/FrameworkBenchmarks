import org.http4k.core.ContentType.Companion.TEXT_PLAIN
import org.http4k.core.Method.GET
import org.http4k.core.Response
import org.http4k.core.Status.Companion.OK
import org.http4k.core.with
import org.http4k.lens.Header.CONTENT_TYPE
import org.http4k.routing.bind

object PlainTextRoute {
    private const val preAllocatedHelloWorldText = "Hello, World!"

    operator fun invoke() = "/plaintext" bind GET to {
        Response(OK).body(preAllocatedHelloWorldText).with(CONTENT_TYPE of TEXT_PLAIN)
    }
}