import com.natpryce.hamkrest.should.shouldMatch
import io.mockk.mockk
import org.http4k.core.Method.GET
import org.http4k.core.Request
import org.http4k.hamkrest.hasBody
import org.junit.jupiter.api.Test

class Http4kBenchmarkServerTest {

    private val database = mockk<Database>()
    private val app = Http4kBenchmarkServer(database)

    @Test
    fun `json`() {
        app(Request(GET, "/json")) shouldMatch hasBody("""{"message":"Hello, World!"}""")
    }

    @Test
    fun `plaintext`() {
        app(Request(GET, "/plaintext")) shouldMatch hasBody("Hello, World!")
    }
}