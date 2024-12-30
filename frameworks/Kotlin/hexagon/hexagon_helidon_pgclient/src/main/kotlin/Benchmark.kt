package com.hexagonkt

import com.hexagonkt.core.Jvm.systemSettingOrNull
import com.hexagonkt.core.media.TEXT_HTML
import com.hexagonkt.core.urlOf
import com.hexagonkt.http.server.helidon.HelidonServerAdapter
import com.hexagonkt.store.BenchmarkPgClientStore
import com.hexagonkt.templates.jte.JteAdapter
import java.time.Duration

fun main() {
    val store = BenchmarkPgClientStore("postgresql")
    val templateEngine = JteAdapter(TEXT_HTML, precompiled = true)
    val templateUrl = urlOf("classpath:fortunes.jte")
    val engine = HelidonServerAdapter(
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
    )

    val benchmark = Benchmark(engine, store, templateEngine, templateUrl, Settings())
    benchmark.server.start()
}
