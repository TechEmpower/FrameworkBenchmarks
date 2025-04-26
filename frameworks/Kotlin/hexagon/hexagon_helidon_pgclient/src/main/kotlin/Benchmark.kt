package com.hexagontk

import com.hexagontk.core.Platform.systemSettingOrNull
import com.hexagontk.core.media.TEXT_HTML
import com.hexagontk.core.urlOf
import com.hexagontk.http.server.helidon.HelidonHttpServer
import com.hexagontk.store.BenchmarkPgClientStore
import com.hexagontk.templates.jte.Jte
import java.time.Duration

fun main() {
    val store = BenchmarkPgClientStore("postgresql")
    val templateEngine = Jte(TEXT_HTML, precompiled = true)
    val templateUrl = urlOf("classpath:fortunes.jte")
    val engine = HelidonHttpServer(
        backlog = systemSettingOrNull("backlog") ?: (8 * 1024),
        writeQueueLength = systemSettingOrNull("writeQueueLength") ?: (8 * 1024),
        readTimeout = Duration.parse(systemSettingOrNull("readTimeout") ?: "PT0S"),
        connectTimeout = Duration.parse(systemSettingOrNull("connectTimeout") ?: "PT0S"),
        tcpNoDelay = systemSettingOrNull<Boolean>("tcpNoDelay") ?: true,
        receiveLog = systemSettingOrNull<Boolean>("receiveLog") ?: false,
        sendLog = systemSettingOrNull<Boolean>("sendLog") ?: false,
        validatePath = systemSettingOrNull<Boolean>("validatePath") ?: false,
        validateRequestHeaders = systemSettingOrNull<Boolean>("validateRequestHeaders") ?: false,
        validateResponseHeaders = systemSettingOrNull<Boolean>("validateResponseHeaders") ?: false,
        smartAsyncWrites = true,
    )

    val benchmark = Benchmark(engine, store, templateEngine, templateUrl, Settings())
    benchmark.server.start()
}
