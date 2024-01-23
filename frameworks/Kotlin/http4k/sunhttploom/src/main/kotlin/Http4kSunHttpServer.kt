import org.http4k.server.SunHttpLoom

fun main() {
    Http4kBenchmarkServer(PostgresDatabase()).start(SunHttpLoom(9000))
}
