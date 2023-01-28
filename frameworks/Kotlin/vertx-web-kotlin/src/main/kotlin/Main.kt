import io.vertx.core.Vertx
import io.vertx.core.impl.cpu.CpuCoreSensor
import io.vertx.kotlin.core.deploymentOptionsOf
import io.vertx.kotlin.core.vertxOptionsOf
import io.vertx.kotlin.coroutines.await

const val SERVER_NAME = "Vert.x Web Kotlin (with kotlinx libraries) Benchmark server"

suspend fun main() {
    println("$SERVER_NAME starting...")
    val vertx = Vertx.vertx(vertxOptionsOf(preferNativeTransport = true))
    vertx.deployVerticle(::MainVerticle, deploymentOptionsOf(instances = CpuCoreSensor.availableProcessors())).await()
    println("$SERVER_NAME started")
}
