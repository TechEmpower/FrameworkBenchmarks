import org.apache.commons.lang3.time.FastDateFormat
import org.http4k.core.Filter
import java.util.TimeZone

fun AddHeaders(addDate: Boolean): Filter {
    val dateFormat = FastDateFormat.getInstance("EEE, d MMM yyyy HH:mm:ss 'GMT'", TimeZone.getTimeZone("GMT"))
    val filter = Filter { next ->
        {
            next(it).let {
                it.headers(
                    listOf(
                        "Server" to "http4k",
                        "Content-Length" to it.body.length.toString(),
                        "Date" to if (addDate) dateFormat.format(System.currentTimeMillis()) else null
                    )
                )
            }
        }
    }
    return filter
}