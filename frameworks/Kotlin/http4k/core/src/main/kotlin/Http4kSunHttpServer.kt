
import org.http4k.server.SunHttp

fun main(args: Array<String>) {
    Http4kBenchmarkServer.start(SunHttp(9000))
}