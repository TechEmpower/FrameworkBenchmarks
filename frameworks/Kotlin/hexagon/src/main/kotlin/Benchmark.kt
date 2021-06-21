package com.hexagonkt

import com.fasterxml.jackson.module.blackbird.BlackbirdModule
import com.hexagonkt.http.server.Server
import com.hexagonkt.http.server.ServerPort
import com.hexagonkt.http.server.ServerSettings
import com.hexagonkt.http.server.jetty.JettyServletAdapter
import com.hexagonkt.serialization.*
import com.hexagonkt.store.BenchmarkMongoDbStore
import com.hexagonkt.store.BenchmarkSqlStore
import com.hexagonkt.store.BenchmarkStore
import com.hexagonkt.templates.TemplatePort
import com.hexagonkt.templates.pebble.PebbleAdapter
import java.net.InetAddress

internal val benchmarkStores: Map<String, BenchmarkStore> by lazy {
    mapOf(
        "mongodb" to BenchmarkMongoDbStore("mongodb"),
        "postgresql" to BenchmarkSqlStore("postgresql")
    )
}

internal val benchmarkTemplateEngines: Map<String, TemplatePort> by lazy {
    mapOf("pebble" to PebbleAdapter)
}

internal val benchmarkEngines: Map<String, ServerPort> by lazy {
    mapOf("jetty" to JettyServletAdapter())
}

internal val benchmarkServer: Server by lazy {
    val settings = Settings()
    val engine = benchmarkEngines[settings.webEngine] ?: error("Unsupported server engine")
    val serverSettings = ServerSettings(
        bindAddress = InetAddress.getByName(settings.bindAddress),
        bindPort = settings.bindPort
    )

    Server(engine, Controller(settings).router, serverSettings)
}

fun main() {
    Json.mapper.registerModule(BlackbirdModule())
    SerializationManager.mapper = JacksonMapper
    SerializationManager.formats = linkedSetOf(Json)

    benchmarkServer.start()
}
