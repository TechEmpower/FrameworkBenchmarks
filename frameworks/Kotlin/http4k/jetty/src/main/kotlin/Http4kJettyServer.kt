
import org.http4k.server.Jetty

fun main(args: Array<String>) {
    Http4kBenchmarkServer.start(Jetty(9000))
}