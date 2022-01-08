package com.hexagonkt

import com.hexagonkt.http.server.HttpServer
import com.hexagonkt.http.server.HttpServerPort
import com.hexagonkt.http.server.HttpServerSettings
import com.hexagonkt.http.server.jetty.JettyServletAdapter
import com.hexagonkt.store.BenchmarkSqlStore
import com.hexagonkt.store.BenchmarkStore
import com.hexagonkt.templates.TemplatePort
import com.hexagonkt.templates.pebble.PebbleAdapter
import java.net.InetAddress

internal val stores: Map<String, BenchmarkStore> by lazy {
    mapOf("postgresql" to BenchmarkSqlStore("postgresql"))
}

internal val templateEngines: Map<String, TemplatePort> by lazy {
    mapOf("pebble" to PebbleAdapter)
}

private val engines: Map<String, HttpServerPort> by lazy {
    mapOf("jetty" to JettyServletAdapter())
}

private val server: HttpServer by lazy {
    val settings = Settings()
    val engine = engines[settings.webEngine] ?: error("Unsupported server engine")
    val controller = Controller(settings, stores, templateEngines)
    val serverSettings = HttpServerSettings(
        bindAddress = InetAddress.getByName(settings.bindAddress),
        bindPort = settings.bindPort,
        options = mapOf(
            "sendDateHeader" to settings.sendDateHeader,
            "sendServerVersion" to settings.sendServerVersion,
            "sendXPoweredBy" to settings.sendXPoweredBy,
        ),
    )

    HttpServer(engine, listOf(controller.path), serverSettings)
}

fun main() {
    server.start()
}
