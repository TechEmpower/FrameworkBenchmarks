import database.r2dbcConnectPool

suspend fun main(args: Array<String>) {
    // Parse CLI arguments
    val isSharedPool = args.getOrNull(0)?.toBooleanStrictOrNull() ?: true
    val poolSize = args.getOrNull(1)?.toIntOrNull() ?: 512
    val useOptimizedConfig = args.getOrNull(2)?.toBooleanStrictOrNull() ?: true

    val benchmarkName = buildString {
        append("Vert.x-Web Kotlinx with Exposed R2DBC (and PostgreSQL)")
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
        commonRunVertxServer(
            benchmarkName,
            { r2dbcConnectPool(poolSize, useOptimizedConfig) },
            ::MainVerticle
        )
    } else {
        commonRunVertxServer(
            benchmarkName,
            { Unit },
            { MainVerticleWithSeparatePool(poolSize, useOptimizedConfig) }
        )
    }
}
