import java.util.Random

interface Database {
    fun findWorld(): World
    fun loadAll(): List<World>
    fun findWorlds(count: Int): List<World>
    fun updateWorlds(count: Int): List<World>
    fun fortunes(): List<Fortune>
}

const val TOTAL_DB_ROWS = 10000

fun Random.world() = nextInt(TOTAL_DB_ROWS - 1) + 1
