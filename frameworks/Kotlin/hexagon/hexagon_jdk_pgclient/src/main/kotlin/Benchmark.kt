package com.hexagontk

import com.hexagontk.core.Platform.systemSettingOrNull
import com.hexagontk.core.media.TEXT_HTML
import com.hexagontk.core.urlOf
import com.hexagontk.http.server.jdk.JdkHttpServer
import com.hexagontk.store.BenchmarkPgClientStore
import com.hexagontk.templates.jte.Jte
import java.util.concurrent.Executors.newVirtualThreadPerTaskExecutor

fun main() {
    System.setProperty("sun.net.httpserver.idleInterval", "5")
    System.setProperty("sun.net.httpserver.maxIdleConnections", "400")

    val settings = Settings()
    val store = BenchmarkPgClientStore("postgresql")
    val templateEngine = Jte(TEXT_HTML, precompiled = true)
    val templateUrl = urlOf("classpath:fortunes.jte")
    val engine = JdkHttpServer(
        executor = newVirtualThreadPerTaskExecutor(),
        backlog = systemSettingOrNull("backlog") ?: (8 * 1024),
    )

    val benchmark = Benchmark(engine, store, templateEngine, templateUrl, settings)
    benchmark.server.start()
}
