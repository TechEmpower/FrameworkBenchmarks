package com.hexagonkt

import com.hexagonkt.core.urlOf
import com.hexagonkt.http.server.nima.NimaServerAdapter
import com.hexagonkt.store.BenchmarkSqlStore
import com.hexagonkt.templates.rocker.RockerAdapter

fun main() {
    val settings = Settings()
    val store = BenchmarkSqlStore("postgresql")
    val templateEngine = RockerAdapter()
    val templateUrl = urlOf("classpath:fortunes.rocker.html")
    val engine = NimaServerAdapter()

    val benchmark = Benchmark(engine, store, templateEngine, templateUrl, settings)
    benchmark.server.start()
}
