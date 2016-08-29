package co.there4.hexagon

import co.there4.hexagon.serialization.parse
import co.there4.hexagon.web.Client
import org.asynchttpclient.Response
import org.testng.annotations.BeforeClass
import org.testng.annotations.Test
import java.net.URL

internal const val THREADS = 4
internal const val TIMES = 16

@Test (threadPoolSize = THREADS, invocationCount = TIMES)
class BenchmarkTest {
    private val client = Client(URL("http://localhost:9090"))

    @BeforeClass fun warmup() {
        main(arrayOf())

        val warmupRounds = if (THREADS > 1) 5 else 0
        (1 ..warmupRounds).forEach {
            json ()
            plaintext ()
            no_query_parameter ()
            empty_query_parameter ()
            text_query_parameter ()
            zero_queries ()
            one_thousand_queries ()
            one_query ()
            ten_queries ()
            one_hundred_queries ()
            five_hundred_queries ()
            fortunes ()
            no_updates_parameter ()
            empty_updates_parameter ()
            text_updates_parameter ()
            zero_updates ()
            one_thousand_updates ()
            one_update ()
            ten_updates ()
            one_hundred_updates ()
            five_hundred_updates ()
        }
    }

    fun json () {
        val response = client.get ("/json")
        val content = response.responseBody

        checkResponse (response, "application/json")
        assert ("Hello, World!" == content.parse(Map::class)["message"])
    }

    fun plaintext () {
        val response = client.get ("/plaintext")
        val content = response.responseBody

        checkResponse (response, "text/plain")
        assert ("Hello, World!" == content)
    }

    fun no_query_parameter () {
        val response = client.get ("/db")
        val content = response.responseBody

        checkResponse (response, "application/json")
        val resultsMap = content.parse(Map::class)
        assert (resultsMap.containsKey ("id") && resultsMap.containsKey ("randomNumber"))
    }

    fun fortunes () {
        val response = client.get ("/fortune")
        val content = response.responseBody
        val contentType = response.headers ["Content-Type"]

        assert (response.headers ["Server"] != null)
        assert (response.headers ["Date"] != null)
        assert (content.contains ("&lt;script&gt;alert(&quot;This should not be displayed"))
        assert (content.contains ("フレームワークのベンチマーク"))
        assert (contentType.toLowerCase ().contains ("text/html"))
    }

    fun no_updates_parameter () {
        val response = client.get ("/update")
        val content = response.responseBody

        checkResponse (response, "application/json")
        val resultsMap = content.parse(Map::class)
        assert (resultsMap.containsKey ("id") && resultsMap.containsKey ("randomNumber"))
    }

    fun empty_query_parameter () = checkDbRequest ("/query?queries", 1)
    fun text_query_parameter () = checkDbRequest ("/query?queries=text", 1)
    fun zero_queries () = checkDbRequest ("/query?queries=0", 1)
    fun one_thousand_queries () = checkDbRequest ("/query?queries=1000", 500)
    fun one_query () = checkDbRequest ("/query?queries=1", 1)
    fun ten_queries () = checkDbRequest ("/query?queries=10", 10)
    fun one_hundred_queries () = checkDbRequest ("/query?queries=100", 100)
    fun five_hundred_queries () = checkDbRequest ("/query?queries=500", 500)

    fun empty_updates_parameter () = checkDbRequest ("/update?queries", 1)
    fun text_updates_parameter () = checkDbRequest ("/update?queries=text", 1)
    fun zero_updates () = checkDbRequest ("/update?queries=0", 1)
    fun one_thousand_updates () = checkDbRequest ("/update?queries=1000", 500)
    fun one_update () = checkDbRequest ("/update?queries=1", 1)
    fun ten_updates () = checkDbRequest ("/update?queries=10", 10)
    fun one_hundred_updates () = checkDbRequest ("/update?queries=100", 100)
    fun five_hundred_updates () = checkDbRequest ("/update?queries=500", 500)

    private fun checkDbRequest (path: String, itemsCount: Int) {
        val response = client.get (path)
        val content = response.responseBody

        checkResponse (response, "application/json")
        checkResultItems (content, itemsCount)
    }

    private fun checkResponse (res: Response, contentType: String) {
        assert(res.headers ["Server"] != null)
        assert(res.headers ["Transfer-Encoding"] != null)
        assert(res.headers ["Content-Type"].contains (contentType))
    }

    private fun checkResultItems (result: String, size: Int) {
        val resultsList = result.parse(List::class)
        assert (size == resultsList.size)

        (1..size).forEach {
            val r = resultsList[it - 1] as Map<*, *>
            assert (r.containsKey ("id") && r.containsKey ("randomNumber"))
        }
    }
}
