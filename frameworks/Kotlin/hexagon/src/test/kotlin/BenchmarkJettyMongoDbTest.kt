package com.hexagonkt

import org.junit.jupiter.api.BeforeAll
import org.junit.jupiter.api.TestInstance
import org.testcontainers.containers.BindMode.READ_ONLY

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class BenchmarkJettyMongoDbTest : BenchmarkTestBase("jetty", "mongodb") {

    @BeforeAll
    fun setUpDataBase() {
        val mongoDb: DockerContainer = DockerContainer("mongo:4.4-bionic")
            .withExposedPorts(27017)
            .withEnv("MONGO_INITDB_DATABASE", "hello_world")
            .withFileSystemBind("$dbResources/mongodb.js", "/docker-entrypoint-initdb.d/mongodb.js", READ_ONLY)
            .apply { start() }

        System.setProperty("MONGODB_DB_HOST", "localhost:${mongoDb.getMappedPort(27017)}")
    }
}