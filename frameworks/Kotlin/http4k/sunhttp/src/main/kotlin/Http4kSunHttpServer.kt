import org.http4k.server.SunHttp

fun main() {
    Http4kBenchmarkServer(PostgresDatabase("tfb-database")).start(SunHttp(9000))
}