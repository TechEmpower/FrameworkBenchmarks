package com.hexagonkt

import com.hexagonkt.core.Jvm.systemFlag
import com.hexagonkt.core.urlOf
import com.hexagonkt.http.server.jetty.JettyServletAdapter
import com.hexagonkt.store.BenchmarkPgClientStore
import com.hexagonkt.templates.rocker.RockerAdapter

fun main() {
    val settings = Settings()
    val store = BenchmarkPgClientStore("postgresql")
    val templateEngine = RockerAdapter()
    val templateUrl = urlOf("classpath:fortunes.rocker.html")
    val engine = JettyServletAdapter(
        sendDateHeader = settings.sendDateHeader,
        sendServerVersion = settings.sendServerVersion,
        sendXPoweredBy = settings.sendXPoweredBy,
        useVirtualThreads = systemFlag("virtualThreads"),
    )

    val benchmark = Benchmark(engine, store, templateEngine, templateUrl, settings)
    benchmark.server.start()
}
