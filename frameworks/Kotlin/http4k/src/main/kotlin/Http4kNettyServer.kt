import org.http4k.server.Netty

fun main(args: Array<String>) {
    Http4kBenchmarkServer.start(Netty(9000))
}