
import com.natpryce.hamkrest.should.shouldMatch
import io.mockk.every
import io.mockk.mockk
import org.http4k.core.Method.GET
import org.http4k.core.Request
import org.http4k.format.Jackson.number
import org.http4k.format.Jackson.obj
import org.http4k.hamkrest.hasBody
import org.junit.jupiter.api.Test
import java.sql.Connection
import java.sql.PreparedStatement

class Http4kBenchmarkServerTest {

    private val database = mockk<Database>()
    private val app = Http4kBenchmarkServer(true, database)

    @Test
    fun `json`() {
        app(Request(GET, "/json")) shouldMatch hasBody("""{"message":"Hello, World!"}""")
    }

    @Test
    fun `plaintext`() {
        app(Request(GET, "/plaintext")) shouldMatch hasBody("Hello, World!")
    }

    @Test
    fun `single`() {
        connectionReturns(obj("id" to number(1), "randomNumber" to number(2)))
        app(Request(GET, "/db")) shouldMatch hasBody("""{"id":1,"randomNumber":2}""")
    }

    @Test
    fun `queries`() {
        connectionReturns(listOf(obj("id" to number(1), "randomNumber" to number(2))))
        app(Request(GET, "/queries").query("queries", "500")) shouldMatch hasBody("""[{"id":1,"randomNumber":2}]""")
    }

    @Test
    fun `updates`() {
        connectionReturns(listOf(obj("id" to number(1), "randomNumber" to number(2))))
        app(Request(GET, "/updates").query("queries", "500")) shouldMatch hasBody("""[{"id":1,"randomNumber":2}]""")
    }

    @Test
    fun `fortunes`() {
        statementReturns(listOf(Fortune(1, "hello world")))
        app(Request(GET, "/fortunes")) shouldMatch hasBody(this.javaClass.getResourceAsStream("/expectedfortunes.html").reader().readText())
    }

    private fun <T> connectionReturns(t: T) = every { database.withConnection(any<(Connection) -> T>()) } returns t
    private fun <T> statementReturns(t: T) = every { database.withStatement(any(), any<(PreparedStatement) -> T>()) } returns t

}