package com.hexagonkt

import com.hexagonkt.http.server.servlet.ServletServer
import javax.servlet.annotation.WebListener

@WebListener class WebListenerServer(settings: Settings = Settings()) : ServletServer(Controller(settings).router) {
    val webRouter = super.router
}