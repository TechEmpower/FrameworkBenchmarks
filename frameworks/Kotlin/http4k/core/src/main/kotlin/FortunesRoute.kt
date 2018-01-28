import org.http4k.core.Body
import org.http4k.core.ContentType.Companion.TEXT_HTML
import org.http4k.core.Method.GET
import org.http4k.core.Response
import org.http4k.core.Status.Companion.OK
import org.http4k.core.with
import org.http4k.routing.bind
import org.http4k.template.HandlebarsTemplates
import org.http4k.template.ViewModel
import org.http4k.template.view

data class Fortune(val id: Int, val message: String)

data class FortunesList(val items: List<Fortune>) : ViewModel

object FortunesRoute {

    private val viewBody = Body.view(HandlebarsTemplates().CachingClasspath(), TEXT_HTML)

    operator fun invoke(database: Database) = "/fortunes" bind GET to {
        val items = database.withStatement("select * from fortune") {
            executeQuery().toList {
                Fortune(getInt(1), getString(2))
            }
        }
            .plus(Fortune(0, "Additional fortune added at request time."))
            .sortedBy { it.message }
        Response(OK).with(viewBody of FortunesList(items))
    }
}