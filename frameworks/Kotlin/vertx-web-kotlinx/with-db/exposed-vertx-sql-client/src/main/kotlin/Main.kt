import com.huanshankeji.exposedvertxsqlclient.ExperimentalEvscApi
import com.huanshankeji.exposedvertxsqlclient.postgresql.exposed.exposedDatabaseConnectPostgresql
import database.connectionConfig

@OptIn(ExperimentalEvscApi::class)
suspend fun main() =
    commonRunVertxServer(
        "Vert.x-Web Kotlinx with Exposed Vert.x SQL Client (and PostgreSQL)",
        { connectionConfig.exposedDatabaseConnectPostgresql() },
        ::MainVerticle
    )
