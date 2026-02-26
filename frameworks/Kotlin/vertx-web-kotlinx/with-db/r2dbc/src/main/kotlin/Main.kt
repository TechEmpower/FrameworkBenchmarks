import database.connectionPoolKtorR2dbc
import database.connectionPoolOriginal
import io.r2dbc.spi.ConnectionFactory

suspend fun main(args: Array<String>) {
    // Parse CLI arguments
    val isSharedPool = args.getOrNull(0)?.toBooleanStrictOrNull() ?: false
    val poolSize = args.getOrNull(1)?.toIntOrNull()
        ?: 8 // TODO `1` might produce equivalent results because there is no transaction in this portion compared to `exposed-r2dbc`.
    val useKtorR2dbcConfig = args.getOrNull(2)?.toBooleanStrictOrNull() ?: false

    val benchmarkName = buildString {
        append("Vert.x-Web Kotlinx with R2DBC (and PostgreSQL)")
        if (!isSharedPool || poolSize != 512 || !useKtorR2dbcConfig) {
            append(" - ")
            if (isSharedPool) {
                append("Shared Pool Size $poolSize")
            } else {
                append("Separate Pool Size $poolSize")
            }
            if (useKtorR2dbcConfig) {
                append(" Ktor R2DBC portion config")
            }
        }
    }

    if (isSharedPool) {
        // Shared pool: create one ConnectionPool that all verticles will share
        val connectionFactory: ConnectionFactory = if (useKtorR2dbcConfig) {
            connectionPoolKtorR2dbc(poolSize)
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
            { MainVerticleWithSeparatePool(poolSize, useKtorR2dbcConfig) }
        )
    }
}
