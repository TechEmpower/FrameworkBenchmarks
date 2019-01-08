
import org.http4k.server.Jetty

fun main(args: Array<String>) {
    Http4kBenchmarkServer(false).start(KtorCIO(9000))
}