package com.hexagonkt

import com.hexagonkt.helpers.Jvm.systemSetting

data class Settings(
    val bindPort: Int = systemSetting("bindPort") ?: 9090,
    val bindAddress: String = "0.0.0.0",

    val database: String = "hello_world",
    val worldCollection: String = "world",
    val fortuneCollection: String = "fortune",

    val databaseUsername: String = "benchmarkdbuser",
    val databasePassword: String = "benchmarkdbpass",

    val maximumPoolSize: Int = systemSetting("maximumPoolSize") ?: 96,

    val webEngine: String = systemSetting("WEBENGINE") ?: "jetty",

    val worldName: String = systemSetting("worldCollection") ?: "world",
    val fortuneName: String = systemSetting("fortuneCollection") ?: "fortune",
    val databaseName: String = systemSetting("database") ?: "hello_world",

    val worldRows: Int = 10_000,
    val textMessage: String = "Hello, World!",
    val queriesParam: String = "queries",
    val cachedQueriesParam: String = "count",
)