package com.hexagonkt

import com.hexagonkt.helpers.Jvm.systemSetting
import com.hexagonkt.http.server.Server
import com.hexagonkt.http.server.ServerPort
import com.hexagonkt.http.server.jetty.JettyServletAdapter
import com.hexagonkt.serialization.*
import com.hexagonkt.settings.SettingsManager.settings
import com.hexagonkt.store.BenchmarkMongoDbStore
import com.hexagonkt.store.BenchmarkSqlStore
import com.hexagonkt.store.BenchmarkStore
import com.hexagonkt.templates.TemplatePort
import com.hexagonkt.templates.pebble.PebbleAdapter

internal val benchmarkStores: Map<String, BenchmarkStore> by lazy {
    mapOf(
        "mongodb" to BenchmarkMongoDbStore("mongodb"),
        "postgresql" to BenchmarkSqlStore("postgresql")
    )
}

internal val benchmarkTemplateEngines: Map<String, TemplatePort> by lazy {
    mapOf("pebble" to PebbleAdapter)
}

internal val engine by lazy { createEngine() }

internal fun createEngine(): ServerPort = when (systemSetting("WEBENGINE") ?: "jetty") {
    "jetty" -> JettyServletAdapter()
    else -> error("Unsupported server engine")
}

internal val benchmarkServer: Server by lazy {
    Server(engine, Controller(Settings()).router, settings.parameters.toObject())
}

fun main() {
    SerializationManager.mapper = JacksonMapper
    SerializationManager.formats = linkedSetOf(Json, Yaml)

    benchmarkServer.start()
}
