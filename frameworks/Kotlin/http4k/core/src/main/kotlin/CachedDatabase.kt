import org.cache2k.Cache
import org.cache2k.Cache2kBuilder
import java.util.Random

class CachedDatabase(private val delegate: Database) : Database by delegate {

    private val random = Random()

    private val cache = object : Cache2kBuilder<Int, Int>() {}
        .name("cachedWorld")
        .eternal(true)
        .entryCapacity(TOTAL_DB_ROWS.toLong())
        .build()
        .apply { refresh() }

    private fun Cache<Int, Int>.refresh() = putAll(delegate.loadAll().toMap())

    override fun findWorlds(count: Int) = (1..count).mapNotNull {
        val randomWorld = random.world()
        cache.peek(randomWorld)?.let { randomWorld to it }
    }

    override fun updateWorlds(count: Int) =
        delegate.updateWorlds(count).onEach { cache.put(it.first, it.second) }

    override fun loadAll() = cache.asMap().toList()
}