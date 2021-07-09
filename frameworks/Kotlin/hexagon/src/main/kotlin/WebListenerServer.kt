package com.hexagonkt

import com.fasterxml.jackson.module.blackbird.BlackbirdModule
import com.hexagonkt.http.server.servlet.ServletServer
import com.hexagonkt.serialization.JacksonMapper
import com.hexagonkt.serialization.Json
import com.hexagonkt.serialization.SerializationManager
import javax.servlet.annotation.WebListener

@WebListener class WebListenerServer(settings: Settings = Settings()) : ServletServer(Controller(settings).router) {

    init {
        Json.mapper.registerModule(BlackbirdModule())
        SerializationManager.mapper = JacksonMapper
        SerializationManager.formats = linkedSetOf(Json)
    }

    val webRouter = super.router
}