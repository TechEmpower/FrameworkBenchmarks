package com.hexagonkt

import com.hexagonkt.core.helpers.multiMapOf
import com.hexagonkt.http.server.handlers.HttpHandler
import com.hexagonkt.http.server.handlers.OnHandler
import com.hexagonkt.http.server.servlet.ServletServer
import jakarta.servlet.annotation.WebListener

@WebListener class WebListenerServer(settings: Settings = Settings()) : ServletServer(createHandlers(settings)) {

    private companion object {

        fun createHandlers(settings: Settings): List<HttpHandler> {
            val controller = Controller(settings, stores, templateEngines)
            val controllerPath = controller.path
            val serverHeaderHandler = OnHandler {
                send(headers = multiMapOf("server" to "Tomcat"))
            }

            return listOf(serverHeaderHandler, controllerPath)
        }
    }
}
