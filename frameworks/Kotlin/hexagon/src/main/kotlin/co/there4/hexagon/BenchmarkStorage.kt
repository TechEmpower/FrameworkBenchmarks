package co.there4.hexagon

import co.there4.hexagon.repository.MongoIdRepository
import co.there4.hexagon.repository.mongoCollection
import java.lang.System.getenv

import co.there4.hexagon.settings.SettingsManager.setting
import co.there4.hexagon.repository.mongoDatabase
import kotlin.reflect.KProperty1

internal val FORTUNE_MESSAGES = setOf(
    "fortune: No such file or directory",
    "A computer scientist is someone who fixes things that aren't broken.",
    "After enough decimal places, nobody gives a damn.",
    "A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1",
    "A computer program does what you tell it to do, not what you want it to do.",
    "Emacs is a nice operating system, but I prefer UNIX. — Tom Christaensen",
    "Any program that runs right is obsolete.",
    "A list is only as strong as its weakest link. — Donald Knuth",
    "Feature: A bug with seniority.",
    "Computers make very fast, very accurate mistakes.",
    "<script>alert(\"This should not be displayed in a browser alert box.\");</script>",
    "フレームワークのベンチマーク"
)

internal val DB_ROWS = 10000

private val DB_HOST = getenv("DBHOST") ?: "localhost"
private val DB = setting<String>("database") ?: "hello_world"
private val WORLD: String = setting<String>("worldCollection") ?: "world"
private val FORTUNE: String = setting<String>("fortuneCollection") ?: "fortune"

private val database = mongoDatabase("mongodb://$DB_HOST/$DB")

internal val worldRepository = repository(WORLD, World::_id)
internal val fortuneRepository = repository(FORTUNE, Fortune::_id)

// TODO Find out why it fails when creating index '_id' with background: true
private inline fun <reified T : Any> repository(name: String, key: KProperty1<T, Int>) =
    MongoIdRepository(T::class, mongoCollection(name, database), key, indexOrder = null)

internal fun initialize() {
    if (fortuneRepository.isEmpty()) {
        val fortunes = FORTUNE_MESSAGES.mapIndexed { ii, fortune -> Fortune(ii + 1, fortune) }
        fortuneRepository.insertManyObjects(fortunes)
    }

    if (worldRepository.isEmpty()) {
        val world = (1..DB_ROWS).map { World(it, it) }
        worldRepository.insertManyObjects(world)
    }
}

internal fun findFortunes() = fortuneRepository.findObjects().toList()

internal fun findWorld() = worldRepository.find(rnd())

internal fun replaceWorld(newWorld: World) {
    worldRepository.replaceObject(newWorld)
}
