
import org.http4k.server.Jetty

fun main(args: Array<String>) {
    Http4kBenchmarkServer(false).start(Jetty(9000))
}