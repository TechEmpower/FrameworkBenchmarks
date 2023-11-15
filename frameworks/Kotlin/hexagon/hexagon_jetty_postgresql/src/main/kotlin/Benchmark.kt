package com.hexagonkt

import com.hexagonkt.core.Jvm.systemFlag
import com.hexagonkt.core.media.TEXT_HTML
import com.hexagonkt.core.urlOf
import com.hexagonkt.http.server.jetty.JettyServletAdapter
import com.hexagonkt.store.BenchmarkSqlStore
import com.hexagonkt.templates.jte.JteAdapter

fun main() {
    val settings = Settings()
    val store = BenchmarkSqlStore("postgresql")
    val templateEngine = JteAdapter(TEXT_HTML, precompiled = true)
    val templateUrl = urlOf("classpath:fortunes.jte")
    val engine = JettyServletAdapter(
        sendDateHeader = settings.sendDateHeader,
        sendServerVersion = settings.sendServerVersion,
        sendXPoweredBy = settings.sendXPoweredBy,
        useVirtualThreads = systemFlag("virtualThreads"),
    )

    val benchmark = Benchmark(engine, store, templateEngine, templateUrl, settings)
    benchmark.server.start()
}
