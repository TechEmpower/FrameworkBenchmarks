import org.http4k.server.Ratpack

fun main() {
    Http4kBenchmarkServer(PostgresDatabase("tfb-database")).start(Ratpack(8080))
}