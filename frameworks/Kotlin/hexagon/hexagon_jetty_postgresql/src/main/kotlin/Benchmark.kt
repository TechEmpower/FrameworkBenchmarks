package com.hexagonkt

import com.hexagonkt.core.Jvm.systemFlag
import com.hexagonkt.http.server.jetty.JettyServletAdapter
import com.hexagonkt.store.BenchmarkSqlStore
import com.hexagonkt.templates.rocker.RockerAdapter
import java.net.URL

fun main() {
    val settings = Settings()
    val store = BenchmarkSqlStore("postgresql")
    val templateEngine = RockerAdapter()
    val templateUrl = URL("classpath:fortunes.rocker.html")
    val engine = JettyServletAdapter(
        sendDateHeader = settings.sendDateHeader,
        sendServerVersion = settings.sendServerVersion,
        sendXPoweredBy = settings.sendXPoweredBy,
        useVirtualThreads = systemFlag("virtualThreads"),
    )

    val benchmark = Benchmark(engine, store, templateEngine, templateUrl, settings)
    benchmark.server.start()
}
