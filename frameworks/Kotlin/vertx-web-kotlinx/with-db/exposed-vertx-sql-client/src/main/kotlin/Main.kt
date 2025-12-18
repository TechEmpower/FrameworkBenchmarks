import com.huanshankeji.exposedvertxsqlclient.postgresql.exposed.exposedDatabaseConnectPostgresql
import database.connectionConfig

suspend fun main(args: Array<String>) =
    commonRunVertxServer(
        "Vert.x-Web Kotlinx with Exposed Vert.x SQL Client (and PostgreSQL)",
        { connectionConfig.exposedDatabaseConnectPostgresql() },
        ::MainVerticle
    )
