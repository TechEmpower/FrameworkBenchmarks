import database.connectionPoolOptimized
import database.connectionPoolOriginal
import io.r2dbc.spi.ConnectionFactory
import kotlinx.coroutines.reactive.awaitSingle

suspend fun main(args: Array<String>) {
    // Parse CLI arguments
    val isSharedPool = args.getOrNull(0)?.toBooleanStrictOrNull() ?: true
    val poolSize = args.getOrNull(1)?.toIntOrNull() ?: 512
    val useOptimizedConfig = args.getOrNull(2)?.toBooleanStrictOrNull() ?: true

    val benchmarkName = buildString {
        append("Vert.x-Web Kotlinx with R2DBC (and PostgreSQL)")
        if (!isSharedPool || poolSize != 512 || !useOptimizedConfig) {
            append(" - ")
            if (isSharedPool) {
                append("Shared Pool Size $poolSize")
            } else {
                append("Separate Pool Size $poolSize")
            }
            if (useOptimizedConfig) {
                append(" Optimized")
            }
        }
    }

    if (isSharedPool) {
        // Shared pool: create one ConnectionPool that all verticles will share
        val connectionFactory: ConnectionFactory = if (useOptimizedConfig) {
            connectionPoolOptimized(poolSize)
        } else {
            connectionPoolOriginal(poolSize)
        }
        
        commonRunVertxServer(
            benchmarkName,
            {},
            { MainVerticle(connectionFactory) }
        )
    } else {
        // Separate pool: each verticle creates its own pool
        commonRunVertxServer(
            benchmarkName,
            {},
            { MainVerticleWithSeparatePool(poolSize, useOptimizedConfig) }
        )
    }
}
