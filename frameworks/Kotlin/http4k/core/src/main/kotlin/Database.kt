import argo.jdom.JsonNode
import org.cache2k.Cache
import org.cache2k.Cache2kBuilder
import java.util.Random

interface Database {
    fun findWorld(): JsonNode
    fun loadAll(): Map<Int, JsonNode>
    fun findWorlds(count: Int): List<JsonNode>
    fun updateWorlds(count: Int): List<JsonNode>
    fun fortunes(): List<Fortune>
}

private const val TOTAL_DB_ROWS = 10000

class CachedDatabase(private val delegate: Database) : Database by delegate {
    private val cache = object : Cache2kBuilder<Int, JsonNode>() {}
        .name("cachedWorld")
        .eternal(true)
        .entryCapacity(TOTAL_DB_ROWS.toLong())
        .build()
        .apply {
            refresh()
        }

    private fun Cache<Int, JsonNode>.refresh() {
        putAll(delegate.loadAll())
    }

    override fun findWorlds(count: Int) = (1..count).map { cache.peek(randomWorld()) }

    override fun updateWorlds(count: Int) =
        delegate.updateWorlds(count).onEach { cache.put(it.getNumberValue("id").toInt(), it) }

    override fun loadAll(): Map<Int, JsonNode> = cache.asMap()
}

fun randomWorld() = Random().nextInt(TOTAL_DB_ROWS - 1) + 1
