package com.hexagonkt

import com.hexagonkt.async.Benchmark
import com.hexagonkt.async.store.BenchmarkPgClientStore
import com.hexagonkt.http.server.netty.async.NettyServerAdapter
import com.hexagonkt.templates.rocker.RockerAdapter
import io.netty.util.ResourceLeakDetector
import io.netty.util.ResourceLeakDetector.Level.DISABLED
import java.net.URL

fun main() {
    ResourceLeakDetector.setLevel(DISABLED)

    System.setProperty("io.netty.buffer.checkBounds", "false")
    System.setProperty("io.netty.buffer.checkAccessible", "false")

    val settings = Settings()
    val store = BenchmarkPgClientStore("postgresql")
    val templateEngine = RockerAdapter()
    val templateUrl = URL("classpath:fortunes.rocker.html")
    val engine = NettyServerAdapter(bossGroupThreads = 48)

    val benchmark = Benchmark(engine, store, templateEngine, templateUrl, settings)
    benchmark.server.start()
}
