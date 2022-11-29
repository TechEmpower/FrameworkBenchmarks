package com.hexagonkt

import com.hexagonkt.core.Jvm.systemSettingOrNull

data class Settings(
    val bindPort: Int = systemSettingOrNull("bindPort") ?: 9090,
    val bindAddress: String = "0.0.0.0",

    val database: String = "hello_world",
    val worldCollection: String = "world",
    val fortuneCollection: String = "fortune",

    val databaseUsername: String = "benchmarkdbuser",
    val databasePassword: String = "benchmarkdbpass",

    val maximumPoolSize: Int = systemSettingOrNull("maximumPoolSize") ?: 96,

    val webEngine: String = systemSettingOrNull("WEBENGINE") ?: "jetty",
    val dataStore: String = systemSettingOrNull("DATASTORE") ?: "postgresql",

    val worldName: String = systemSettingOrNull("worldCollection") ?: "world",
    val fortuneName: String = systemSettingOrNull("fortuneCollection") ?: "fortune",
    val databaseName: String = systemSettingOrNull("database") ?: "hello_world",
    val databaseDriver: String = systemSettingOrNull("databaseDriver") ?: "org.postgresql.Driver",

    val sendDateHeader: Boolean = systemSettingOrNull("sendDateHeader") ?: false,
    val sendServerVersion: Boolean = systemSettingOrNull("sendServerVersion") ?: false,
    val sendXPoweredBy: Boolean = systemSettingOrNull("sendXPoweredBy") ?: false,

    val worldRows: Int = 10_000,
    val textMessage: String = "Hello, World!",
    val queriesParam: String = "queries",
    val cachedQueriesParam: String = "count",
)
