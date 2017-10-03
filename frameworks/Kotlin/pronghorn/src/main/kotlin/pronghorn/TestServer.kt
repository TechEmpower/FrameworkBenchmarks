package pronghorn

import tech.pronghorn.http.HttpResponses
import tech.pronghorn.http.protocol.CommonContentTypes
import tech.pronghorn.server.HttpServer
import tech.pronghorn.server.handlers.StaticHttpRequestHandler

fun main(args: Array<String>) {
    val helloWorldResponse = HttpResponses.OK("Hello, World!", CommonContentTypes.TextPlain)
    val helloWorldHandler = StaticHttpRequestHandler(helloWorldResponse)

    val server = HttpServer("0.0.0.0", 8080)
    server.registerUrlHandler("/plaintext", helloWorldHandler)
    server.registerUrlHandlerGenerator("/json", { JsonHandler() })
    server.start()
}
