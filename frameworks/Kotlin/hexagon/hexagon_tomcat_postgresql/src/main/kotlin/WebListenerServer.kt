package com.hexagonkt

import com.hexagonkt.http.model.Header
import com.hexagonkt.http.model.Headers
import com.hexagonkt.http.server.handlers.HttpHandler
import com.hexagonkt.http.server.handlers.OnHandler
import com.hexagonkt.http.server.servlet.ServletServer
import com.hexagonkt.store.BenchmarkSqlStore
import com.hexagonkt.templates.rocker.RockerAdapter
import jakarta.servlet.annotation.WebListener
import java.net.URL

@WebListener class WebListenerServer(
    settings: Settings = Settings()
) : ServletServer(createHandlers(settings)) {

    private companion object {
        val headers = Headers(Header("server", "Tomcat"))

        fun createHandlers(settings: Settings): List<HttpHandler> {
            val store = BenchmarkSqlStore("postgresql")
            val templateEngine = RockerAdapter()
            val templateUrl = URL("classpath:fortunes.rocker.html")
            val controller = Controller(settings, store, templateEngine, templateUrl)
            val controllerPath = controller.path
            val serverHeaderHandler = OnHandler("*") {
                send(headers = headers)
            }

            return listOf(serverHeaderHandler, controllerPath)
        }
    }
}
