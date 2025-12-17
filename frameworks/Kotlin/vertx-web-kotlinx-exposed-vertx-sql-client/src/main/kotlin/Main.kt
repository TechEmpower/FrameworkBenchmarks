import com.huanshankeji.exposedvertxsqlclient.postgresql.exposed.exposedDatabaseConnectPostgresql
import database.connectionConfig
import io.vertx.core.Vertx
import io.vertx.core.impl.cpu.CpuCoreSensor
import io.vertx.kotlin.core.deploymentOptionsOf
import io.vertx.kotlin.core.vertxOptionsOf
import io.vertx.kotlin.coroutines.coAwait
import java.util.function.Supplier
import java.util.logging.Logger

const val BENCHMARK_NAME = "Vert.x-Web Kotlinx with Exposed Vert.x SQL Client Benchmark"
const val SERVER_NAME = "$BENCHMARK_NAME server"
val numProcessors = CpuCoreSensor.availableProcessors()

val logger = Logger.getLogger(BENCHMARK_NAME)
suspend fun main(args: Array<String>) {
    logger.info("$SERVER_NAME starting...")
    val vertx = Vertx.vertx(
        vertxOptionsOf(
            eventLoopPoolSize = numProcessors, preferNativeTransport = true, disableTCCL = true
        )
    )
    vertx.exceptionHandler {
        logger.info("Vertx exception caught: $it")
        it.printStackTrace()
    }
    val exposedDatabase = connectionConfig.exposedDatabaseConnectPostgresql()
    vertx.deployVerticle(
        Supplier { MainVerticle(exposedDatabase) },
        deploymentOptionsOf(instances = numProcessors)
    ).coAwait()
    logger.info("$SERVER_NAME started.")
}
