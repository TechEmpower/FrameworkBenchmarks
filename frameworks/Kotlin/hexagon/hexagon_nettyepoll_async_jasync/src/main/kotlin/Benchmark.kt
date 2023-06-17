package com.hexagonkt

import com.hexagonkt.async.Benchmark
import com.hexagonkt.async.store.BenchmarkJasyncStore
import com.hexagonkt.http.server.netty.epoll.async.NettyEpollServerAdapter
import com.hexagonkt.templates.rocker.RockerAdapter
import io.netty.util.ResourceLeakDetector
import io.netty.util.ResourceLeakDetector.Level.DISABLED
import java.net.URL

fun main() {
    ResourceLeakDetector.setLevel(DISABLED)

    System.setProperty("io.netty.buffer.checkBounds", "false")
    System.setProperty("io.netty.buffer.checkAccessible", "false")

    val settings = Settings()
    val store = BenchmarkJasyncStore("postgresql")
    val templateEngine = RockerAdapter()
    val templateUrl = URL("classpath:fortunes.rocker.html")
    val engine = NettyEpollServerAdapter(bossGroupThreads = 48)

    val benchmark = Benchmark(engine, store, templateEngine, templateUrl, settings)
    benchmark.server.start()
}
