package pronghorn.handlers

import pronghorn.utils.MongoDBHandlerHelpers
import tech.pronghorn.http.*
import tech.pronghorn.http.protocol.CommonContentTypes
import tech.pronghorn.server.HttpServerWorker

class MongoDBRequestSingleHandler(worker: HttpServerWorker) : MongoDBHandlerHelpers(worker) {
    private val collection = getWorldCollection()

    suspend override fun handle(exchange: HttpExchange): HttpResponse {
        val world = findWorld(collection)
        return HttpResponses.OK(toJson(world), CommonContentTypes.ApplicationJson)
    }
}
