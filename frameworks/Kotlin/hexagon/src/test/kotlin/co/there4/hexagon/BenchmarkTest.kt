package co.there4.hexagon

import co.there4.hexagon.rest.HttpClient
import co.there4.hexagon.serialization.parse
import okhttp3.Response
import org.testng.annotations.BeforeClass
import org.testng.annotations.Test
import java.net.URL

@Test (threadPoolSize = 4, invocationCount = 8)
class BenchmarkTest {
    private val WARM_UP = 10
    private val ENDPOINT = "http://localhost:5050"

    private var application: Benchmark? = null
    private val client = HttpClient(URL(ENDPOINT))

    @BeforeClass fun setup () {
        application = Benchmark ()

        (1..WARM_UP).forEach {
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

    fun get (url: String) = client.get(url)
    fun getContent (res: Response) = res.body().string()
    fun parse (data: String) = data.parse(Map::class)

    fun json () {
        val response = get ("/json")
        val content = getContent(response)

        checkResponse (response, content, "application/json")
        assert ("Hello, World!" == parse(content)["message"])
    }

    fun plaintext () {
        val response = get ("/plaintext")
        val content = getContent (response)

        checkResponse (response, content, "text/plain")
        assert ("Hello, World!" == content)
    }

    fun no_query_parameter () {
        val response = get ("/db")
        val content = getContent (response)

        checkResponse (response, content, "application/json")
        val resultsMap = parse(content)
        assert (resultsMap.containsKey ("id") && resultsMap.containsKey ("randomNumber"))
    }

    fun empty_query_parameter () {
        checkDbRequest ("/query?queries", 1)
    }

    fun text_query_parameter () {
        checkDbRequest ("/query?queries=text", 1)
    }

    fun zero_queries () {
        checkDbRequest ("/query?queries=0", 1)
    }

    fun one_thousand_queries () {
        checkDbRequest ("/query?queries=1000", 500)
    }

    fun one_query () {
        checkDbRequest ("/query?queries=1", 1)
    }

    fun ten_queries () {
        checkDbRequest ("/query?queries=10", 10)
    }

    fun one_hundred_queries () {
        checkDbRequest ("/query?queries=100", 100)
    }

    fun five_hundred_queries () {
        checkDbRequest ("/query?queries=500", 500)
    }

    fun fortunes () {
        val response = get ("/fortune")
        val content = getContent (response)
        val contentType = response.header ("Content-Type")

        assert (response.header ("Server") != null)
        assert (response.header ("Date") != null)
        assert (content.contains ("&lt;script&gt;alert(&quot;This should not be displayed"))
        assert (content.contains ("フレームワークのベンチマーク"))
        assert (contentType.toLowerCase ().contains ("text/html"))
    }

    fun no_updates_parameter () {
        val response = get ("/update")
        val content = getContent (response)

        checkResponse (response, content, "application/json")
        val resultsMap = parse(content)
        assert (resultsMap.containsKey ("id") && resultsMap.containsKey ("randomNumber"))
    }

    fun empty_updates_parameter () {
        checkDbRequest ("/update?queries", 1)
    }

    fun text_updates_parameter () {
        checkDbRequest ("/update?queries=text", 1)
    }

    fun zero_updates () {
        checkDbRequest ("/update?queries=0", 1)
    }

    fun one_thousand_updates () {
        checkDbRequest ("/update?queries=1000", 500)
    }

    fun one_update () {
        checkDbRequest ("/update?queries=1", 1)
    }

    fun ten_updates () {
        checkDbRequest ("/update?queries=10", 10)
    }

    fun one_hundred_updates () {
        checkDbRequest ("/update?queries=100", 100)
    }

    fun five_hundred_updates () {
        checkDbRequest ("/update?queries=500", 500)
    }

    private fun checkDbRequest (path: String, itemsCount: Int) {
        val response = client.get (path)
        val content = response.body().string()

        checkResponse (response, content, "application/json")
        checkResultItems (content, itemsCount)
    }

    private fun checkResponse (res: Response, content: String, contentType: String) {
        assert(res.header ("Server") != null)
        assert(res.header ("Transfer-Encoding") != null)
        assert(res.header ("Content-Type").contains (contentType))
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
