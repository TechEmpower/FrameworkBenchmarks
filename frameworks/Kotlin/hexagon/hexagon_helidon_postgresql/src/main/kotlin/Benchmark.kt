package com.hexagontk

import com.hexagontk.core.media.TEXT_HTML
import com.hexagontk.core.urlOf
import com.hexagontk.http.server.helidon.HelidonHttpServer
import com.hexagontk.store.BenchmarkSqlStore
import com.hexagontk.templates.jte.Jte

fun main() {
    val settings = Settings()
    val store = BenchmarkSqlStore("postgresql")
    val templateEngine = Jte(TEXT_HTML, precompiled = true)
    val templateUrl = urlOf("classpath:fortunes.jte")
    val engine = HelidonHttpServer()

    val benchmark = Benchmark(engine, store, templateEngine, templateUrl, settings)
    benchmark.server.start()
}
