import com.huanshankeji.exposedvertxsqlclient.postgresql.exposed.exposedDatabaseConnectPostgresql
import database.connectionConfig
import io.vertx.core.Vertx
import io.vertx.core.impl.cpu.CpuCoreSensor
import io.vertx.kotlin.core.deploymentOptionsOf
import io.vertx.kotlin.core.vertxOptionsOf
import io.vertx.kotlin.coroutines.coAwait
import java.util.function.Supplier
import java.util.logging.Logger

const val SERVER_NAME = "Vert.x-Web Kotlinx Benchmark server"
val numProcessors = CpuCoreSensor.availableProcessors()

val logger = Logger.getLogger("Vert.x-Web Kotlinx Benchmark")
suspend fun main(args: Array<String>) {
    val hasDb = args.getOrNull(0)?.toBooleanStrictOrNull()
        ?: throw IllegalArgumentException("Specify the first `hasDb` Boolean argument")

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
    val exposedDatabase = if (hasDb) connectionConfig.exposedDatabaseConnectPostgresql() else null
    vertx.deployVerticle(
        Supplier { MainVerticle(exposedDatabase) },
        deploymentOptionsOf(instances = numProcessors)
    ).coAwait()
    logger.info("$SERVER_NAME started.")
}
