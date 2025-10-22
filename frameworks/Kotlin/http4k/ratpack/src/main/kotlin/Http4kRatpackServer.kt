import org.http4k.server.Ratpack

fun main() {
    Http4kBenchmarkServer(PostgresDatabase()).start(Ratpack(9000))
}