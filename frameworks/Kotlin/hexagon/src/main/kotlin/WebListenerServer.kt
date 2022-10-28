package com.hexagonkt

import com.hexagonkt.http.model.Header
import com.hexagonkt.http.model.HttpFields
import com.hexagonkt.http.server.handlers.HttpHandler
import com.hexagonkt.http.server.handlers.OnHandler
import com.hexagonkt.http.server.servlet.ServletServer
import com.hexagonkt.store.BenchmarkSqlStore
import com.hexagonkt.templates.pebble.PebbleAdapter
import jakarta.servlet.annotation.WebListener

@WebListener class WebListenerServer(settings: Settings = Settings()) : ServletServer(createHandlers(settings)) {

    private companion object {
        val headers = HttpFields(Header("server", "Tomcat"))

        fun createHandlers(settings: Settings): List<HttpHandler> {
            val controller = Controller(settings, BenchmarkSqlStore("postgresql"), PebbleAdapter)
            val controllerPath = controller.path
            val serverHeaderHandler = OnHandler("*") {
                send(headers = headers)
            }

            return listOf(serverHeaderHandler, controllerPath)
        }
    }
}
