package com.hexagonkt

import com.hexagonkt.core.media.TEXT_HTML
import com.hexagonkt.core.urlOf
import com.hexagonkt.http.server.helidon.HelidonServerAdapter
import com.hexagonkt.store.BenchmarkPgClientStore
import com.hexagonkt.templates.jte.JteAdapter

fun main() {
    val settings = Settings()
    val store = BenchmarkPgClientStore("postgresql")
    val templateEngine = JteAdapter(TEXT_HTML, precompiled = true)
    val templateUrl = urlOf("classpath:fortunes.jte")
    val engine = HelidonServerAdapter()

    val benchmark = Benchmark(engine, store, templateEngine, templateUrl, settings)
    benchmark.server.start()
}
