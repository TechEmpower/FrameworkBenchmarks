import org.http4k.core.Body
import org.http4k.core.ContentType.Companion.TEXT_HTML
import org.http4k.core.Method.GET
import org.http4k.core.Response
import org.http4k.core.Status.Companion.OK
import org.http4k.core.with
import org.http4k.routing.bind
import org.http4k.template.RockerTemplates
import org.http4k.template.viewModel

data class Fortune(val id: Int, val message: String)

object FortunesRoute {
    private val viewBody = Body.viewModel(RockerTemplates().Caching(), TEXT_HTML).toLens()

    operator fun invoke(db: Database) = "/fortunes" bind GET to {
        Response(OK).with(viewBody of FortunesList().items(db.fortunes()))
    }
}
