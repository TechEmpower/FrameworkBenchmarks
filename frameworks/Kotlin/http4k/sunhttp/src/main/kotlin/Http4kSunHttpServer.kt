import org.http4k.server.SunHttp

fun main() {
    Http4kBenchmarkServer(PostgresDatabase()).start(SunHttp(9000))
}