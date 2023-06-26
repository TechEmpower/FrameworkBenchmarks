import io.vertx.core.Vertx
import io.vertx.core.impl.cpu.CpuCoreSensor
import io.vertx.kotlin.core.deploymentOptionsOf
import io.vertx.kotlin.core.vertxOptionsOf
import io.vertx.kotlin.coroutines.await
import java.util.logging.Logger

const val SERVER_NAME = "Vert.x-Web Kotlinx Benchmark"

val logger = Logger.getLogger("Vert.x-Web Kotlinx Benchmark")
suspend fun main(args: Array<String>) {
    val hasDb = args.getOrNull(0)?.toBooleanStrictOrNull()
        ?: throw IllegalArgumentException("Specify the first `hasDb` Boolean argument")

    logger.info("$SERVER_NAME starting...")
    val vertx = Vertx.vertx(vertxOptionsOf(preferNativeTransport = true))
    vertx.exceptionHandler {
        logger.info("Vertx exception caught: $it")
        it.printStackTrace()
    }
    vertx.deployVerticle({ MainVerticle(hasDb) }, deploymentOptionsOf(instances = CpuCoreSensor.availableProcessors()))
        .await()
    logger.info("$SERVER_NAME started.")
}
