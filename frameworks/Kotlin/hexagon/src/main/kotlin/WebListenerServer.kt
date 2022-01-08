package com.hexagonkt

import com.hexagonkt.http.server.servlet.ServletServer
import jakarta.servlet.annotation.WebListener

@WebListener class WebListenerServer(settings: Settings = Settings()) :
    ServletServer(listOf(Controller(settings, stores, templateEngines).path))
