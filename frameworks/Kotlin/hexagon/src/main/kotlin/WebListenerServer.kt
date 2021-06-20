package com.hexagonkt

import com.hexagonkt.http.server.servlet.ServletServer
import javax.servlet.annotation.WebListener

@WebListener class WebListenerServer : ServletServer(Controller(Settings()).router) {
    val webRouter = super.router
}