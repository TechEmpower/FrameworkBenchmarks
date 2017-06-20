import org.http4k.server.Undertow

fun main(args: Array<String>) {
    Http4kBenchmarkServer.start(Undertow(9000))
}