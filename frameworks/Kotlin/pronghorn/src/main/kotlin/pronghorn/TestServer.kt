package pronghorn

import ch.qos.logback.classic.Level
import com.jsoniter.output.EncodingMode
import com.jsoniter.output.JsonStream
import pronghorn.handlers.*
import pronghorn.utils.TestConfig
import pronghorn.utils.setLibraryLogging
import tech.pronghorn.http.HttpResponses
import tech.pronghorn.http.protocol.CommonContentTypes
import tech.pronghorn.server.HttpServer
import tech.pronghorn.server.requesthandlers.StaticHttpRequestHandler

fun main(args: Array<String>) {
    JsonStream.setMode(EncodingMode.DYNAMIC_MODE) // enable faster Json encoding mode
    setLibraryLogging(Level.WARN) // minimize logging from chatty libraries

    val helloWorldResponse = HttpResponses.OK("Hello, World!", CommonContentTypes.TextPlain)
    val helloWorldHandler = StaticHttpRequestHandler(helloWorldResponse)

    val server = HttpServer(TestConfig.listenHost, TestConfig.listenPort)
    server.registerUrlHandler("/plaintext", helloWorldHandler)
    server.registerUrlHandlerGenerator("/json", { JsonHandler() })
    server.registerUrlHandlerGenerator("/db", { worker -> MongoDBRequestSingleHandler(worker) })
    server.registerUrlHandlerGenerator("/queries", { worker -> MongoDBRequestMultiHandler(worker) })
    server.registerUrlHandlerGenerator("/fortunes", { worker -> MongoDBRequestFortunesHandler(worker) })
    server.registerUrlHandlerGenerator("/updates", { worker -> MongoDBRequestUpdatesHandler(worker) })
    server.start()
}
