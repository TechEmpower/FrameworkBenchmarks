package com.hexagonkt

import com.hexagonkt.core.media.TEXT_HTML
import com.hexagonkt.core.urlOf
import com.hexagonkt.http.model.Header
import com.hexagonkt.http.model.Headers
import com.hexagonkt.http.handlers.HttpHandler
import com.hexagonkt.http.handlers.OnHandler
import com.hexagonkt.http.handlers.PathHandler
import com.hexagonkt.http.server.servlet.ServletServer
import com.hexagonkt.store.BenchmarkSqlStore
import com.hexagonkt.templates.jte.JteAdapter
import jakarta.servlet.annotation.WebListener

@WebListener class WebListenerServer(
    settings: Settings = Settings()
) : ServletServer(createHandlers(settings)) {

    private companion object {
        val headers = Headers(Header("server", "Tomcat"))

        fun createHandlers(settings: Settings): HttpHandler {
            val store = BenchmarkSqlStore("postgresql")
            val templateEngine = JteAdapter(TEXT_HTML, precompiled = true)
            val templateUrl = urlOf("classpath:fortunes.jte")
            val controller = Controller(settings, store, templateEngine, templateUrl)
            val controllerPath = controller.path
            val serverHeaderHandler = OnHandler("*") {
                send(headers = headers)
            }

            return PathHandler(serverHeaderHandler, controllerPath)
        }
    }
}
