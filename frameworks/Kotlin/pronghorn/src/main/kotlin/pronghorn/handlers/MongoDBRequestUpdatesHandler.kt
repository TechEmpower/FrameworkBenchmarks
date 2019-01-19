package pronghorn.handlers

import pronghorn.types.World
import pronghorn.utils.MongoDBHandlerHelpers
import tech.pronghorn.coroutines.awaitable.InternalFuture
import tech.pronghorn.coroutines.awaitable.await
import tech.pronghorn.http.*
import tech.pronghorn.http.protocol.CommonContentTypes
import tech.pronghorn.server.HttpServerWorker

class MongoDBRequestUpdatesHandler(worker: HttpServerWorker) : MongoDBHandlerHelpers(worker) {
    private val collection = getWorldCollection()

    suspend override fun handle(exchange: HttpExchange): HttpResponse {
        val future = InternalFuture<Array<World>>()
        val worldCount = getQueryCount(exchange)
        findWorlds(collection, future.promise(), worldCount)
        val results = await(future)
        results.forEach { world -> world.randomNumber = random.nextInt(10000) + 1 }
        writeWorlds(collection, results)
        return HttpResponses.OK(toJson(results), CommonContentTypes.ApplicationJson)
    }
}
