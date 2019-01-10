
import org.http4k.server.SunHttp

fun main(args: Array<String>) {
    Http4kBenchmarkServer(PostgresDatabase("tfb-database")).start(SunHttp(9000))
}