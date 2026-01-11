import io.vertx.core.Verticle
import io.vertx.core.Vertx
import io.vertx.core.impl.cpu.CpuCoreSensor
import io.vertx.kotlin.core.deploymentOptionsOf
import io.vertx.kotlin.core.vertxOptionsOf
import io.vertx.kotlin.coroutines.coAwait
import java.util.function.Supplier
import java.util.logging.Logger

val numProcessors = CpuCoreSensor.availableProcessors()

val logger = Logger.getLogger("Vert.x-Web Kotlinx Benchmark")
suspend fun <SharedResources> commonRunVertxServer(
    benchmarkName: String,
    createSharedResources: (Vertx) -> SharedResources,
    createVerticle: (SharedResources) -> Verticle
) {
    val serverName = "$benchmarkName benchmark server"
    logger.info("$serverName starting...")
    val vertx = Vertx.vertx(
        vertxOptionsOf(
            eventLoopPoolSize = numProcessors, preferNativeTransport = true, disableTCCL = true
        )
    )
    vertx.exceptionHandler {
        logger.info("Vertx exception caught: $it")
        it.printStackTrace()
    }
    val sharedResources = createSharedResources(vertx)
    vertx.deployVerticle(
        Supplier { createVerticle(sharedResources) },
        deploymentOptionsOf(instances = numProcessors)
    ).coAwait()
    logger.info("$serverName started.")
}
