package com.hexagonkt

import com.hexagonkt.http.server.HttpServer
import com.hexagonkt.http.server.HttpServerPort
import com.hexagonkt.http.server.HttpServerSettings
import com.hexagonkt.http.server.jetty.JettyServletAdapter
import com.hexagonkt.http.server.netty.NettyServerAdapter
import com.hexagonkt.http.server.netty.epoll.NettyEpollServerAdapter
import com.hexagonkt.store.BenchmarkPgClientStore
import com.hexagonkt.store.BenchmarkSqlStore
import com.hexagonkt.store.BenchmarkStore
import com.hexagonkt.templates.pebble.PebbleAdapter
import java.net.InetAddress

internal val settings = Settings()

private val engines: Map<String, HttpServerPort> by lazy {
    mapOf(
        "jetty" to JettyServletAdapter(
            sendDateHeader = settings.sendDateHeader,
            sendServerVersion = settings.sendServerVersion,
            sendXPoweredBy = settings.sendXPoweredBy,
        ),
        "jetty_loom" to JettyServletAdapter(
            sendDateHeader = settings.sendDateHeader,
            sendServerVersion = settings.sendServerVersion,
            sendXPoweredBy = settings.sendXPoweredBy,
            useVirtualThreads = true,
        ),
        "netty" to NettyServerAdapter(),
        "netty_epoll" to NettyEpollServerAdapter(),
    )
}

private val stores: Map<String, BenchmarkStore> by lazy {
    mapOf(
        "postgresql" to BenchmarkSqlStore("postgresql"),
        "pg_client" to BenchmarkPgClientStore("postgresql"),
    )
}

internal val server: HttpServer by lazy {
    val engine = engines[settings.webEngine] ?: error("Unsupported server engine")
    val store = stores[settings.dataStore] ?: error("Unsupported data store")
    val controller = Controller(settings, store, PebbleAdapter())
    val serverSettings = HttpServerSettings(
        bindAddress = InetAddress.getByName(settings.bindAddress),
        bindPort = settings.bindPort,
    )

    HttpServer(engine, controller.path, serverSettings)
}

fun main() {
    server.start()
}
