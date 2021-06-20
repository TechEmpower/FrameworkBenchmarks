package com.hexagonkt

import com.hexagonkt.helpers.println
import com.hexagonkt.http.Method
import com.hexagonkt.http.client.Client
import com.hexagonkt.http.client.Response
import com.hexagonkt.http.client.ahc.AhcAdapter
import com.hexagonkt.http.server.jetty.JettyServletAdapter
import com.hexagonkt.serialization.Json
import com.hexagonkt.serialization.parse
import com.hexagonkt.serialization.parseObjects
import org.junit.jupiter.api.AfterAll
import org.junit.jupiter.api.BeforeAll
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.TestInstance
import org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS
import java.io.File
import java.lang.IllegalStateException
import kotlin.test.assertFailsWith

@TestInstance(PER_CLASS)
abstract class BenchmarkTestBase(
    private val webEngine: String,
    private val databaseEngine: String,
    private val templateEngine: String = "pebble"
) {

    private val endpoint = System.getProperty("verify.endpoint")
    private val users = (System.getProperty("users") ?: "8").toInt()
    private val count = (System.getProperty("count") ?: "2").toInt() * users

    internal val dbResources = File("src/test/resources").canonicalPath

    private lateinit var client: Client

    private fun checkResponse(res: Response, contentType: String) {
        assert(res.headers["Date"] != null)
        assert(res.headers["Server"] != null)
        assert(res.headers["Transfer-Encoding"] != null)
        assert(res.headers["Content-Type"]?.first() == contentType)
    }

    private fun checkDbRequest(path: String, itemsCount: Int) {
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

    @BeforeAll
    fun beforeSpec() {
        System.setProperty("WEBENGINE", webEngine)
        client = if (endpoint == null) {
            main()
            Client(AhcAdapter(), "http://localhost:${benchmarkServer.runtimePort}")
        }
        else {
            Client(AhcAdapter(), endpoint)
        }
    }

    @AfterAll
    fun afterSpec() {
        benchmarkStores[databaseEngine]?.close()
        benchmarkServer.stop()
    }

    @Test
    fun `Empty server code creates a Jetty Servlet Adapter`() {
        System.clearProperty("WEBENGINE")
        createEngine()
        assert(engine is JettyServletAdapter)
    }

    @Test
    fun `Invalid server code throws an exception`() {
        assertFailsWith<IllegalStateException> {
            System.setProperty("WEBENGINE", "invalid")
            createEngine()
        }
    }

    @Test
    fun `Web test`() {
        val web = WebListenerServer()

        val webRoutes = web.webRouter.requestHandlers
            .map { it.route.methods.first() to it.route.path.pattern }

        val benchmarkRoutes = listOf(
            Method.GET to "/plaintext",
            Method.GET to "/json",
            Method.GET to "/$databaseEngine/$templateEngine/fortunes",
            Method.GET to "/$databaseEngine/db",
            Method.GET to "/$databaseEngine/query",
            Method.GET to "/$databaseEngine/update"
        )

        assert(webRoutes.containsAll(benchmarkRoutes))
    }

    @Test
    fun `JSON requests`() {//.config(invocations = count, threads = users) {
        val response = client.get("/json")
        val content = response.body

        checkResponse(response, Json.contentType)
        assert("Hello, World!" == content?.parse<Message>()?.message)
    }

    @Test
    fun `Plaintext requests`() {//.config(invocations = count, threads = users) {
        val response = client.get("/plaintext")
        val content = response.body

        checkResponse(response, "text/plain")
        assert("Hello, World!" == content)
    }

    @Test
    fun `Fortunes requests`() {//.config(invocations = count, threads = users) {
        val response = client.get("/$databaseEngine/$templateEngine/fortunes")
        val content = response.body ?: error("body is required")

        checkResponse(response, "text/html;charset=utf-8")
        assert(content.contains("<td>&lt;script&gt;alert(&quot;This should not be"))
        assert(content.contains(" displayed in a browser alert box.&quot;);&lt;/script&gt;</td>"))
        assert(content.contains("<td>フレームワークのベンチマーク</td>"))
    }

    @Test
    fun `No query parameter requests`() {//.config(invocations = count, threads = users) {
        val response = client.get("/$databaseEngine/db")
        val body = response.body ?: error("body is required")

        checkResponse(response, Json.contentType)
        val bodyMap = body.parse(Map::class)
        assert(bodyMap.containsKey(World::id.name))
        assert(bodyMap.containsKey(World::randomNumber.name))
    }

    @Test
    fun `No updates parameter requests`() {//.config(invocations = count, threads = users) {
        val response = client.get("/$databaseEngine/update")
        val body = response.body ?: error("body is required")

        checkResponse(response, Json.contentType)
        val bodyMap = body.parseObjects(Map::class).first()
        assert(bodyMap.containsKey(World::id.name))
        assert(bodyMap.containsKey(World::randomNumber.name))
    }

    @Test
    fun `DB requests`() {
        fun testDb(test: String, path: String, itemsCount: Int) {
//            test.config(invocations = count, threads = users) {
//            }
            test.println()
            checkDbRequest("/$databaseEngine/$path", itemsCount)
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
    }
}