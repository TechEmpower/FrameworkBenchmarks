package com.hexagonkt.async

import com.hexagonkt.Settings
import com.hexagonkt.http.server.async.HttpServer
import com.hexagonkt.http.server.async.HttpServerPort
import com.hexagonkt.http.server.async.HttpServerSettings
import com.hexagonkt.async.store.BenchmarkStore
import com.hexagonkt.templates.TemplatePort
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
