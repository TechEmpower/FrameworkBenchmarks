package com.hexagonkt

import com.hexagonkt.core.urlOf
import com.hexagonkt.http.server.netty.epoll.NettyEpollServerAdapter
import com.hexagonkt.store.BenchmarkSqlStore
import com.hexagonkt.templates.rocker.RockerAdapter
import io.netty.util.ResourceLeakDetector
import io.netty.util.ResourceLeakDetector.Level.DISABLED

fun main() {
    ResourceLeakDetector.setLevel(DISABLED)

    System.setProperty("io.netty.buffer.checkBounds", "false")
    System.setProperty("io.netty.buffer.checkAccessible", "false")

    val settings = Settings()
    val store = BenchmarkSqlStore("postgresql")
    val templateEngine = RockerAdapter()
    val templateUrl = urlOf("classpath:fortunes.rocker.html")
    val engine = NettyEpollServerAdapter()

    val benchmark = Benchmark(engine, store, templateEngine, templateUrl, settings)
    benchmark.server.start()
}
