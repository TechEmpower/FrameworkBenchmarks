package pronghorn.handlers

import httl.Engine
import pronghorn.types.Fortune
import pronghorn.utils.MongoDBHandlerHelpers
import tech.pronghorn.http.*
import tech.pronghorn.http.protocol.CommonContentTypes
import tech.pronghorn.server.HttpServerWorker
import java.io.ByteArrayOutputStream

class MongoDBRequestFortunesHandler(worker: HttpServerWorker) : MongoDBHandlerHelpers(worker) {
    private val collection = getFortunesCollection()
    private val engine = Engine.getEngine()
    private val template = engine.getTemplate("/fortunes.httl")
    private val output = ByteArrayOutputStream()

    suspend override fun handle(exchange: HttpExchange): HttpResponse {
        val fortunes = findFortunes(collection)
        fortunes.add(Fortune(0, "Additional fortune added at request time."))
        output.reset()
        template.render(mapOf("fortunes" to fortunes), output)
        return HttpResponses.OK(output.toByteArray(), CommonContentTypes.TextHtml, Charsets.UTF_8)
    }
}
