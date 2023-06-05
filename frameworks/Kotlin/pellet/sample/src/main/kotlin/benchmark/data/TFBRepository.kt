package benchmark.data

import io.vertx.kotlin.coroutines.await
import io.vertx.pgclient.PgConnectOptions
import io.vertx.pgclient.PgPool
import io.vertx.sqlclient.PoolOptions
import io.vertx.sqlclient.Tuple
import java.util.concurrent.ThreadLocalRandom

class TFBRepository: WorldDAO, FortuneDAO {

    private val connectOptions = PgConnectOptions()
        .setPort(5432)
        .setHost("tfb-database")
        .setDatabase("hello_world")
        .setUser("benchmarkdbuser")
        .setPassword("benchmarkdbpass")
        .apply {
            cachePreparedStatements = true
        }

    private val poolOptions = PoolOptions()
    private val client = PgPool.client(connectOptions, poolOptions)

    override suspend fun fetchWorld(): WorldDTO {
        val worldId = ThreadLocalRandom.current().nextInt(1, 10001)
        val result = client
            .preparedQuery("select id, randomNumber from world where id = $1")
            .execute(Tuple.of(worldId))
            .await()
        val row = result.first()
        return WorldDTO(
            row.getInteger(0),
            row.getInteger(1)
        )
    }

    override suspend fun updateWorlds(worlds: List<WorldDTO>) {
        val batch = worlds.map {
            Tuple.of(it.id, it.randomNumber)
        }
        client
            .preparedQuery("update world set randomNumber = $1 where id = $2")
            .executeBatch(batch)
            .await()
    }

    override suspend fun fetchFortunes(): List<Fortune> {
        val results = client.preparedQuery("select id, message from fortune")
            .execute()
            .await()
        return results.map {
            Fortune(
                it.getInteger(0),
                it.getString(1)
            )
        }
    }
}