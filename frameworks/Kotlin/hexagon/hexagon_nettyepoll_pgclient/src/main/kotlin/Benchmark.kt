package com.hexagontk

import com.hexagontk.core.media.TEXT_HTML
import com.hexagontk.core.urlOf
import com.hexagontk.http.server.netty.epoll.NettyEpollHttpServer
import com.hexagontk.store.BenchmarkPgClientStore
import com.hexagontk.templates.jte.Jte
import io.netty.util.ResourceLeakDetector
import io.netty.util.ResourceLeakDetector.Level.DISABLED

fun main() {
    ResourceLeakDetector.setLevel(DISABLED)

    System.setProperty("io.netty.buffer.checkBounds", "false")
    System.setProperty("io.netty.buffer.checkAccessible", "false")

    val settings = Settings()
    val store = BenchmarkPgClientStore("postgresql")
    val templateEngine = Jte(TEXT_HTML, precompiled = true)
    val templateUrl = urlOf("classpath:fortunes.jte")
    val engine = NettyEpollHttpServer(
        keepAliveHandler = false,
        httpAggregatorHandler = false,
        chunkedHandler = false,
        enableWebsockets = false,
    )

    val benchmark = Benchmark(engine, store, templateEngine, templateUrl, settings)
    benchmark.server.start()
}
