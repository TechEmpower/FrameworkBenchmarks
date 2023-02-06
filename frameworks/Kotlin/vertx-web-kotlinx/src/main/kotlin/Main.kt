import io.vertx.core.Vertx
import io.vertx.core.impl.cpu.CpuCoreSensor
import io.vertx.kotlin.core.deploymentOptionsOf
import io.vertx.kotlin.core.vertxOptionsOf
import io.vertx.kotlin.coroutines.await
import java.util.logging.Logger

const val SERVER_NAME = "Vert.x Web Kotlin (with kotlinx libraries) Benchmark server"

val logger = Logger.getLogger("Vert.x Web Kotlinx Benchmark")
suspend fun main() {
    logger.info("$SERVER_NAME starting...")
    val vertx = Vertx.vertx(vertxOptionsOf(preferNativeTransport = true))
    vertx.exceptionHandler {
        logger.info("Vertx exception caught: $it")
        it.printStackTrace()
    }
    vertx.deployVerticle(::MainVerticle, deploymentOptionsOf(instances = CpuCoreSensor.availableProcessors())).await()
    logger.info("$SERVER_NAME started.")
}
