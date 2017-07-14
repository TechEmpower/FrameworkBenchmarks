import org.http4k.asByteBuffer
import org.http4k.core.Body
import org.http4k.core.ContentType.Companion.TEXT_PLAIN
import org.http4k.core.Method.GET
import org.http4k.core.Response
import org.http4k.core.Status.Companion.OK
import org.http4k.core.with
import org.http4k.lens.binary
import org.http4k.routing.bind

object PlainTextRoute {
    private val preAllocatedHelloWorldText = "Hello, World!".asByteBuffer()

    private val plainTextBody = Body.binary(TEXT_PLAIN).toLens()

    operator fun invoke() = "/plaintext" to GET bind { Response(OK).with(plainTextBody of preAllocatedHelloWorldText) }
}