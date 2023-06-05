package com.hexagonkt

import com.hexagonkt.http.server.netty.NettyServerAdapter
import com.hexagonkt.store.BenchmarkSqlStore
import com.hexagonkt.templates.rocker.RockerAdapter
import java.net.URL

fun main() {
    val settings = Settings()
    val store = BenchmarkSqlStore("postgresql")
    val templateEngine = RockerAdapter()
    val templateUrl = URL("classpath:fortunes.rocker.html")
    val engine = NettyServerAdapter()

    val benchmark = Benchmark(engine, store, templateEngine, templateUrl, settings)
    benchmark.server.start()
}
