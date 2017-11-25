package pronghorn.handlers

import pronghorn.types.World
import pronghorn.utils.JsonSupport
import pronghorn.utils.MongoDBHandlerHelpers
import tech.pronghorn.coroutines.awaitable.InternalFuture
import tech.pronghorn.coroutines.awaitable.await
import tech.pronghorn.http.*
import tech.pronghorn.http.protocol.CommonContentTypes
import tech.pronghorn.server.HttpServerWorker

class MongoDBRequestMultiHandler(worker: HttpServerWorker) : MongoDBHandlerHelpers(worker), JsonSupport {
    private val collection = getWorldCollection()

    suspend override fun handle(exchange: HttpExchange): HttpResponse {
        val future = InternalFuture<Array<World>>()
        val worldCount = getQueryCount(exchange)
        findWorlds(collection, future.promise(), worldCount)
        val results = await(future)
        return HttpResponses.OK(toJson(results), CommonContentTypes.ApplicationJson)
    }
}
