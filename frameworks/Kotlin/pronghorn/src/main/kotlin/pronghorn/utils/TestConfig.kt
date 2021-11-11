package pronghorn.utils

import tech.pronghorn.util.ignoreException
import java.util.Properties

object TestConfig {
    private val properties = parsePropertiesConfig()
    val listenHost = properties.getProperty("listenHost", "0.0.0.0")
    val listenPort = getIntProperty("listenPort", 8080)
    val dbHost = properties.getProperty("dbHost", "tfb-database")
    val dbPort = getIntProperty("dbPort", 27017)
    val dbName = properties.getProperty("dbName", "hello_world")
    val fortunesCollectionName = properties.getProperty("fortunesCollectionName", "fortune")
    val worldCollectionName = properties.getProperty("worldCollectionName", "world")

    private fun getIntProperty(key: String,
                               default: Int): Int {
        val result = properties.getProperty(key)
        return when (result) {
            null -> default
            else -> result.toIntOrNull() ?: default
        }
    }

    private fun parsePropertiesConfig(): Properties {
        val properties = Properties()
        ignoreException {
            val stream = javaClass.classLoader.getResource("benchmark.properties")?.openStream()
            if (stream != null) {
                properties.load(stream)
            }
        }
        return properties
    }
}
