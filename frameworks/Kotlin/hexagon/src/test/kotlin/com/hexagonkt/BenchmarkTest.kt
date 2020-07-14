package com.hexagonkt

import com.hexagonkt.serialization.parse
import com.hexagonkt.http.client.Client
import com.hexagonkt.serialization.Json
import com.hexagonkt.http.Method.GET
import com.hexagonkt.http.client.Response
import com.hexagonkt.http.client.ahc.AhcAdapter
import com.hexagonkt.http.server.jetty.JettyServletAdapter
import com.hexagonkt.serialization.parseObjects
import io.kotest.assertions.throwables.shouldThrow
import io.kotest.core.spec.style.StringSpec
import java.lang.IllegalStateException
import java.lang.System.setProperty

class BenchmarkJettyMongoDbTest : BenchmarkTestBase("jetty", "mongodb")

class BenchmarkJettyPostgreSqlTest : BenchmarkTestBase("jetty", "postgresql")

abstract class BenchmarkTestBase(
    private val webEngine: String,
    private val databaseEngine: String,
    private val templateEngine: String = "pebble"
): StringSpec({

    val endpoint = System.getProperty("verify.endpoint")
    val users = (System.getProperty("users") ?: "8").toInt()
    val count = (System.getProperty("count") ?: "2").toInt() * users

    lateinit var client: Client

    fun checkResponse(res: Response, contentType: String) {
        assert(res.headers["Date"] != null)
        assert(res.headers["Server"] != null)
        assert(res.headers["Transfer-Encoding"] != null)
        assert(res.headers["Content-Type"]?.first() == contentType)
    }

    fun checkDbRequest(path: String, itemsCount: Int) {
        val response = client.get(path)
        val content = response.body

        checkResponse(response, Json.contentType)

        val resultsList = content?.parse(List::class) ?: error("")
        assert(itemsCount == resultsList.size)

        (1..itemsCount).forEach {
            val r = resultsList[it - 1] as Map<*, *>
            assert(r.containsKey(World::id.name) && r.containsKey(World::randomNumber.name))
            assert(!r.containsKey(World::_id.name))
            assert((r[World::id.name] as Int) in 1..10000)
        }
    }

    beforeSpec {
        setProperty("WEBENGINE", webEngine)
        client = if (endpoint == null) {
            main()
            Client(AhcAdapter(), "http://localhost:${benchmarkServer.runtimePort}")
        }
        else {
            Client(AhcAdapter(), endpoint)
        }
    }

    afterSpec {
        benchmarkStores[databaseEngine]?.close()
        benchmarkServer.stop()
    }

    "Empty server code creates a Jetty Servlet Adapter" {
        System.clearProperty("WEBENGINE")
        createEngine()
        assert(engine is JettyServletAdapter)
    }

    "Invalid server code throws an exception" {
        shouldThrow<IllegalStateException> {
            setProperty("WEBENGINE", "invalid")
            createEngine()
        }
    }

    "Web" {
        val web = Web()

        val webRoutes = web.serverRouter.requestHandlers
            .map { it.route.methods.first() to it.route.path.path }

        val benchmarkRoutes = listOf(
            GET to "/plaintext",
            GET to "/json",
            GET to "/$databaseEngine/$templateEngine/fortunes",
            GET to "/$databaseEngine/db",
            GET to "/$databaseEngine/query",
            GET to "/$databaseEngine/update"
        )

        assert(webRoutes.containsAll(benchmarkRoutes))
    }

    "JSON".config(invocations = count, threads = users) {
        val response = client.get("/json")
        val content = response.body

        checkResponse(response, Json.contentType)
        assert("Hello, World!" == content?.parse<Message>()?.message)
    }

    "Plaintext".config(invocations = count, threads = users) {
        val response = client.get("/plaintext")
        val content = response.body

        checkResponse(response, "text/plain")
        assert("Hello, World!" == content)
    }

    "Fortunes".config(invocations = count, threads = users) {
        val response = client.get("/$databaseEngine/$templateEngine/fortunes")
        val content = response.body ?: error("body is required")

        checkResponse(response, "text/html;charset=utf-8")
        assert(content.contains("<td>&lt;script&gt;alert(&quot;This should not be"))
        assert(content.contains(" displayed in a browser alert box.&quot;);&lt;/script&gt;</td>"))
        assert(content.contains("<td>フレームワークのベンチマーク</td>"))
    }

    "No query parameter".config(invocations = count, threads = users) {
        val response = client.get("/$databaseEngine/db")
        val body = response.body ?: error("body is required")

        checkResponse(response, Json.contentType)
        val bodyMap = body.parse(Map::class)
        assert(bodyMap.containsKey(World::id.name))
        assert(bodyMap.containsKey(World::randomNumber.name))
    }

    "No updates parameter".config(invocations = count, threads = users) {
        val response = client.get("/$databaseEngine/update")
        val body = response.body ?: error("body is required")

        checkResponse(response, Json.contentType)
        val bodyMap = body.parseObjects(Map::class).first()
        assert(bodyMap.containsKey(World::id.name))
        assert(bodyMap.containsKey(World::randomNumber.name))
    }

    fun testDb(test: String, path: String, itemsCount: Int) {
        test.config(invocations = count, threads = users) {
            checkDbRequest("/$databaseEngine/$path", itemsCount)
        }
    }

    testDb("Empty query parameter", "query?queries", 1)
    testDb("Text query parameter", "query?queries=text", 1)
    testDb("Zero queries", "query?queries=0", 1)
    testDb("One thousand queries", "query?queries=1000", 500)
    testDb("One query", "query?queries=1", 1)
    testDb("Ten queries", "query?queries=10", 10)
    testDb("One hundred queries", "query?queries=100", 100)
    testDb("Five hundred queries", "query?queries=500", 500)

    testDb("Empty updates parameter", "update?queries", 1)
    testDb("Text updates parameter", "update?queries=text", 1)
    testDb("Zero updates", "update?queries=0", 1)
    testDb("One thousand updates", "update?queries=1000", 500)
    testDb("One update", "update?queries=1", 1)
    testDb("Ten updates", "update?queries=10", 10)
    testDb("One hundred updates", "update?queries=100", 100)
    testDb("Five hundred updates", "update?queries=500", 500)
})
