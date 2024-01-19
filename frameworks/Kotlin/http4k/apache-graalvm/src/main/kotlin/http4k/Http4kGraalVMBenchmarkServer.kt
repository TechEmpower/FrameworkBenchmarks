fun main() {
    Http4kBenchmarkServer(PostgresDatabase()).start(TfbApacheServer(9000))
}
