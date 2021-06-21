package com.hexagonkt

import org.junit.jupiter.api.BeforeAll
import org.junit.jupiter.api.TestInstance
import org.testcontainers.containers.BindMode.READ_ONLY

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class BenchmarkJettyPostgreSqlTest : BenchmarkTestBase("jetty", "postgresql") {

    @BeforeAll
    fun setUpDataBase() {
        val postgreSql: DockerContainer = DockerContainer("postgres:13.3-alpine")
            .withExposedPorts(5432)
            .withEnv("POSTGRES_USER", "benchmarkdbuser")
            .withEnv("POSTGRES_PASSWORD", "benchmarkdbpass")
            .withFileSystemBind("$dbResources/postgresql.sql", "/docker-entrypoint-initdb.d/db.sql", READ_ONLY)
            .apply { start() }

        System.setProperty("POSTGRESQL_DB_HOST", "localhost:${postgreSql.getMappedPort(5432)}")
    }
}