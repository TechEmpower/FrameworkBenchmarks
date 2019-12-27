package com.hexagonkt

import com.hexagonkt.serialization.parse
import com.hexagonkt.http.client.Client
import com.hexagonkt.serialization.Json
import com.hexagonkt.serialization.parseObjects
import com.hexagonkt.http.Method.GET
import com.hexagonkt.http.server.jetty.JettyServletAdapter
import org.asynchttpclient.Response
import org.testng.annotations.AfterClass
import org.testng.annotations.BeforeClass
import org.testng.annotations.Test
import java.lang.IllegalStateException
import java.lang.System.setProperty

@Test class BenchmarkJettyMongoDbTest : BenchmarkTestBase("jetty", "mongodb")

@Test class BenchmarkJettyPostgreSqlTest : BenchmarkTestBase("jetty", "postgresql")

@Test abstract class BenchmarkTestBase(
    private val webEngine: String,
    private val databaseEngine: String,
    private val templateEngine: String = "pebble"
) {
    private val client by lazy { Client("http://localhost:${benchmarkServer.runtimePort}") }

    @BeforeClass fun startUp() {
        setProperty("WEBENGINE", webEngine)
        main()
    }

    @AfterClass fun shutDown() {
        benchmarkStores[databaseEngine]?.close()
        benchmarkServer.stop()
    }

    @Test fun `Empty server code creates a Jetty Servlet Adapter`() {
        System.clearProperty("WEBENGINE")
        createEngine()
        assert(engine is JettyServletAdapter)
    }

    @Test(expectedExceptions = [ IllegalStateException::class ])
    fun `Invalid server code throws an exception`() {
        setProperty("WEBENGINE", "invalid")
        createEngine()
    }

    @Test fun web() {
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

    @Test fun json() {
        val response = client.get("/json")
        val content = response.responseBody

        checkResponse(response, Json.contentType)
        assert("Hello, World!" == content.parse<Message>().message)
    }

    @Test fun plaintext() {
        val response = client.get("/plaintext")
        val content = response.responseBody

        checkResponse(response, "text/plain")
        assert("Hello, World!" == content)
    }

    @Test fun fortunes() {
        val response = client.get("/$databaseEngine/$templateEngine/fortunes")
        val content = response.responseBody

        checkResponse(response, "text/html;charset=utf-8")
        assert(content.contains("<td>&lt;script&gt;alert(&quot;This should not be"))
        assert(content.contains(" displayed in a browser alert box.&quot;);&lt;/script&gt;</td>"))
        assert(content.contains("<td>フレームワークのベンチマーク</td>"))
    }

    @Test fun `no query parameter`() {
        val response = client.get("/$databaseEngine/db")
        val body = response.responseBody

        checkResponse(response, Json.contentType)
        val bodyMap = body.parse(Map::class)
        assert(bodyMap.containsKey(World::id.name))
        assert(bodyMap.containsKey(World::randomNumber.name))
    }

    @Test fun `no updates parameter`() {
        val response = client.get("/$databaseEngine/update")
        val body = response.responseBody

        checkResponse(response, Json.contentType)
        val bodyMap = body.parseObjects(Map::class).first()
        assert(bodyMap.containsKey(World::id.name))
        assert(bodyMap.containsKey(World::randomNumber.name))
    }

    @Test fun `empty query parameter`() = checkDbRequest("/$databaseEngine/query?queries", 1)
    @Test fun `text query parameter`() = checkDbRequest("/$databaseEngine/query?queries=text", 1)
    @Test fun `zero queries`() = checkDbRequest("/$databaseEngine/query?queries=0", 1)
    @Test fun `one thousand queries`() = checkDbRequest("/$databaseEngine/query?queries=1000", 500)
    @Test fun `one query`() = checkDbRequest("/$databaseEngine/query?queries=1", 1)
    @Test fun `ten queries`() = checkDbRequest("/$databaseEngine/query?queries=10", 10)
    @Test fun `one hundred queries`() = checkDbRequest("/$databaseEngine/query?queries=100", 100)
    @Test fun `five hundred queries`() = checkDbRequest("/$databaseEngine/query?queries=500", 500)

    @Test fun `empty updates parameter`() = checkDbRequest("/$databaseEngine/update?queries", 1)
    @Test fun `text updates parameter`() = checkDbRequest("/$databaseEngine/update?queries=text", 1)
    @Test fun `zero updates`() = checkDbRequest("/$databaseEngine/update?queries=0", 1)
    @Test fun `one thousand updates`() = checkDbRequest("/$databaseEngine/update?queries=1000", 500)
    @Test fun `one update`() = checkDbRequest("/$databaseEngine/update?queries=1", 1)
    @Test fun `ten updates`() = checkDbRequest("/$databaseEngine/update?queries=10", 10)
    @Test fun `one hundred updates`() = checkDbRequest("/$databaseEngine/update?queries=100", 100)
    @Test fun `five hundred updates`() = checkDbRequest("/$databaseEngine/update?queries=500", 500)

    private fun checkDbRequest(path: String, itemsCount: Int) {
        val response = client.get(path)
        val content = response.responseBody

        checkResponse(response, Json.contentType)

        val resultsList = content.parse(List::class)
        assert(itemsCount == resultsList.size)

        (1..itemsCount).forEach {
            val r = resultsList[it - 1] as Map<*, *>
            assert(r.containsKey(World::id.name) && r.containsKey(World::randomNumber.name))
            assert(!r.containsKey(World::_id.name))
            assert((r[World::id.name] as Int) in 1..10000)
        }
    }

    private fun checkResponse(res: Response, contentType: String) {
        assert(res.headers ["Date"] != null)
        assert(res.headers ["Server"] != null)
        assert(res.headers ["Transfer-Encoding"] != null)
        assert(res.headers ["Content-Type"] == contentType)
    }
}
