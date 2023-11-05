package com.hexagonkt

import com.hexagonkt.core.media.TEXT_HTML
import com.hexagonkt.core.urlOf
import com.hexagonkt.http.server.netty.epoll.NettyEpollServerAdapter
import com.hexagonkt.store.BenchmarkPgClientStore
import com.hexagonkt.templates.jte.JteAdapter
import io.netty.util.ResourceLeakDetector
import io.netty.util.ResourceLeakDetector.Level.DISABLED

fun main() {
    ResourceLeakDetector.setLevel(DISABLED)

    System.setProperty("io.netty.buffer.checkBounds", "false")
    System.setProperty("io.netty.buffer.checkAccessible", "false")

    val settings = Settings()
    val store = BenchmarkPgClientStore("postgresql")
    val templateEngine = JteAdapter(TEXT_HTML, precompiled = true)
    val templateUrl = urlOf("classpath:fortunes.jte")
    val engine = NettyEpollServerAdapter(
        keepAliveHandler = false,
        httpAggregatorHandler = false,
        chunkedHandler = false,
        enableWebsockets = false,
    )

    val benchmark = Benchmark(engine, store, templateEngine, templateUrl, settings)
    benchmark.server.start()
}
