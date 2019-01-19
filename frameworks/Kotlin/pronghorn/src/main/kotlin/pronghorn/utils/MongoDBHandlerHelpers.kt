package pronghorn.utils

import com.jsoniter.output.JsonStream
import com.mongodb.ServerAddress
import com.mongodb.async.client.*
import com.mongodb.client.model.Filters
import com.mongodb.client.model.UpdateOneModel
import com.mongodb.connection.ClusterSettings
import com.mongodb.connection.ConnectionPoolSettings
import org.bson.Document
import org.bson.codecs.configuration.CodecRegistries
import pronghorn.types.Fortune
import pronghorn.types.World
import pronghorn.types.codecs.FortuneCodec
import pronghorn.types.codecs.WorldCodec
import tech.pronghorn.coroutines.awaitable.InternalFuture
import tech.pronghorn.coroutines.awaitable.await
import tech.pronghorn.http.HttpExchange
import tech.pronghorn.mongodb.MultiplexMongoDBStreamFactoryFactory
import tech.pronghorn.server.HttpServerWorker
import tech.pronghorn.server.requesthandlers.SuspendableHttpRequestHandler
import java.io.ByteArrayOutputStream
import java.util.SplittableRandom
import java.util.TreeSet

private val queriesBytes = "queries".toByteArray(Charsets.US_ASCII)

abstract class MongoDBHandlerHelpers(worker: HttpServerWorker) : SuspendableHttpRequestHandler(), JsonSupport {
    private val mongoClient = worker.getOrPutAttachment(MongoDBClientAttachmentKey, { createMongoClient(worker) })
    protected val random = SplittableRandom()
    override final val outputStream = ByteArrayOutputStream()
    override final val stream = JsonStream(outputStream, 32)

    protected fun getWorldCollection(): MongoCollection<World> {
        return mongoClient
                .getDatabase(TestConfig.dbName)
                .getCollection(TestConfig.worldCollectionName)
                .withDocumentClass(World::class.java)
    }

    protected fun getFortunesCollection(): MongoCollection<Fortune> {
        return mongoClient
                .getDatabase(TestConfig.dbName)
                .getCollection(TestConfig.fortunesCollectionName)
                .withDocumentClass(Fortune::class.java)
    }

    private fun createMongoClient(worker: HttpServerWorker): MongoClient {
        val poolSettings = ConnectionPoolSettings.builder()
                .minSize(1)
                .maxSize(Int.MAX_VALUE) // connections are multiplexed via Pronghorn Stream, so max size is irrelevant
                .maxWaitQueueSize(0)
                .build()

        val clusterSettings = ClusterSettings.builder()
                .hosts(listOf(ServerAddress(TestConfig.dbHost, TestConfig.dbPort)))
                .build()

        val multiplexedFactory = MultiplexMongoDBStreamFactoryFactory(worker)

        val codecRegistry = CodecRegistries.fromRegistries(
                MongoClients.getDefaultCodecRegistry(),
                CodecRegistries.fromCodecs(FortuneCodec, WorldCodec)
        )

        val settings = MongoClientSettings.builder()
                .clusterSettings(clusterSettings)
                .connectionPoolSettings(poolSettings)
                .streamFactoryFactory(multiplexedFactory)
                .codecRegistry(codecRegistry)
                .build()

        val client = MongoClients.create(settings)
        worker.registerShutdownMethod {
            client.close()
        }
        return client
    }

    protected fun getQueryCount(exchange: HttpExchange): Int {
        return Math.max(1, Math.min(500, exchange.requestUrl.getQueryParamAsInt(queriesBytes) ?: 1))
    }

    suspend protected fun findWorld(collection: MongoCollection<World>): World {
        val future = InternalFuture<World>()
        val promise = future.promise()
        val id = random.nextInt(10000) + 1
        collection.find(Document("_id", id)).first { world, ex ->
            when {
                world != null -> promise.complete(world)
                ex != null -> promise.completeExceptionally(ex)
                else -> promise.completeExceptionally(Exception("Missing document."))
            }
        }
        return await(future)
    }

    @Suppress("UNCHECKED_CAST")
    protected fun findWorlds(collection: MongoCollection<World>,
                             promise: InternalFuture.InternalPromise<Array<World>>,
                             count: Int,
                             results: Array<World?> = arrayOfNulls(count)) {
        val id = count
        val searchDocument = Document("_id", id)
        collection.find(searchDocument).first { world, ex ->
            when {
                world != null -> {
                    results[count - 1] = world
                    if (count == 1) {
                        promise.complete(results as Array<World>)
                    }
                    else {
                        findWorlds(collection, promise, count - 1, results)
                    }
                }
                ex != null -> promise.completeExceptionally(ex)
                else -> promise.completeExceptionally(Exception("Missing document."))
            }
        }
    }

    protected suspend fun findFortunes(collection: MongoCollection<Fortune>): TreeSet<Fortune> {
        val future = InternalFuture<Unit>()
        val promise = future.promise()
        val fortunes = TreeSet<Fortune>()

        collection.find().into(fortunes, { _, _ -> promise.complete(Unit) })

        await(future)
        return fortunes
    }

    protected suspend fun writeWorlds(collection: MongoCollection<World>,
                                      results: Array<World>) {
        val updateFuture = InternalFuture<Unit>()
        val updatePromise = updateFuture.promise()

        val updates = results.map { result ->
            UpdateOneModel<World>(
                    Filters.eq("_id", result.id),
                    Document("\$set", Document("randomNumber", result.randomNumber))
            )
        }

        collection.bulkWrite(updates, { result, ex ->
            when {
                result != null -> updatePromise.complete(Unit)
                ex != null -> updatePromise.completeExceptionally(ex)
                else -> updatePromise.completeExceptionally(Exception("Unexpected update failure."))
            }
        })

        await(updateFuture)
    }
}
