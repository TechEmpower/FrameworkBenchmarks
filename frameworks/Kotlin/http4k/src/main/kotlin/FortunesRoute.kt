
import org.http4k.core.Method.GET
import org.http4k.core.Response
import org.http4k.core.Status.Companion.OK
import org.http4k.routing.Route
import org.http4k.routing.by
import org.http4k.template.HandlebarsTemplates
import org.http4k.template.ViewModel

data class Fortune(val id: Int, val message: String)

data class FortunesList(val items: List<Fortune>) : ViewModel

object FortunesRoute {

    private val renderer = HandlebarsTemplates().CachingClasspath()

    operator fun invoke(database: Database): Route = GET to "fortunes" by {
        val items = database.withConnection {
            it.prepareStatement("select * from fortune").executeQuery().toList {
                Fortune(it.getInt(1), it.getString(2))
            }
        }
        Response(OK).body(renderer(FortunesList(items)))
    }
}