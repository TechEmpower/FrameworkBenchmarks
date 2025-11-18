package com.hexagontk

import com.hexagontk.core.media.TEXT_HTML
import com.hexagontk.core.urlOf
import com.hexagontk.http.model.Field
import com.hexagontk.http.model.Headers
import com.hexagontk.http.handlers.HttpHandler
import com.hexagontk.http.handlers.OnHandler
import com.hexagontk.http.handlers.PathHandler
import com.hexagontk.http.server.servlet.ServletServer
import com.hexagontk.store.BenchmarkSqlStore
import com.hexagontk.templates.jte.Jte
import jakarta.servlet.annotation.WebListener

@WebListener class WebListenerServer(
    settings: Settings = Settings()
) : ServletServer(createHandlers(settings)) {

    private companion object {
        val headers = Headers(Field("server", "Tomcat"))

        fun createHandlers(settings: Settings): HttpHandler {
            val store = BenchmarkSqlStore("postgresql")
            val templateEngine = Jte(TEXT_HTML, precompiled = true)
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
