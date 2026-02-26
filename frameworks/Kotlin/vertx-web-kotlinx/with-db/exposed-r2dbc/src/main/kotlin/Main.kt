import database.r2dbcConnectPool

suspend fun main(args: Array<String>) {
    // Parse CLI arguments
    val isSharedPool = args.getOrNull(0)?.toBooleanStrictOrNull() ?: false
    val poolSize = args.getOrNull(1)?.toIntOrNull() ?: 8
    val useKtorR2dbcConfig = args.getOrNull(2)?.toBooleanStrictOrNull() ?: false

    val benchmarkName = buildString {
        append("Vert.x-Web Kotlinx with Exposed R2DBC (and PostgreSQL)")
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
        commonRunVertxServer(
            benchmarkName,
            { r2dbcConnectPool(poolSize, useKtorR2dbcConfig) },
            ::MainVerticle
        )
    } else {
        commonRunVertxServer(
            benchmarkName,
            { Unit },
            { MainVerticleWithSeparatePool(poolSize, useKtorR2dbcConfig) }
        )
    }
}
