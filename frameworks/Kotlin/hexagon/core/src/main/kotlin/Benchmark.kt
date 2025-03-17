package com.hexagontk

import com.hexagontk.http.server.HttpServer
import com.hexagontk.http.server.HttpServerPort
import com.hexagontk.http.server.HttpServerSettings
import com.hexagontk.store.BenchmarkStore
import com.hexagontk.templates.TemplatePort
import java.net.InetAddress
import java.net.URL

class Benchmark(
    private val engine: HttpServerPort,
    private val store: BenchmarkStore,
    private val template: TemplatePort,
    private val templateUrl: URL,
    private val settings: Settings = Settings(),
) {
    val server: HttpServer by lazy {
        val controller = Controller(settings, store, template, templateUrl)
        val serverSettings = HttpServerSettings(
            bindAddress = InetAddress.getByName(settings.bindAddress),
            bindPort = settings.bindPort,
        )

        HttpServer(engine, controller.path, serverSettings)
    }
}
