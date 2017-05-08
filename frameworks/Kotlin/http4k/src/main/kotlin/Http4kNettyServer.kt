import org.http4k.core.HttpHandler
import org.http4k.core.Response
import org.http4k.core.Status.Companion.OK
import org.http4k.server.asNettyServer

fun main(args: Array<String>) {
    val svc: HttpHandler = { Response(OK).body("hello") }

    svc.asNettyServer(8000).start().block()
}