package com.hexagonkt

import com.hexagonkt.http.server.netty.epoll.NettyEpollServerAdapter
import com.hexagonkt.store.BenchmarkPgClientStore
import com.hexagonkt.templates.rocker.RockerAdapter
import java.net.URL

fun main() {
    val settings = Settings()
    val store = BenchmarkPgClientStore("postgresql")
    val templateEngine = RockerAdapter()
    val templateUrl = URL("classpath:fortunes.rocker.html")
    val engine = NettyEpollServerAdapter()

    val benchmark = Benchmark(engine, store, templateEngine, templateUrl, settings)
    benchmark.server.start()
}
