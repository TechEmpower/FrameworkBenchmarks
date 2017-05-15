import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}

object currentTime {
  private[this] val formatter: DateTimeFormatter = 
    DateTimeFormatter.RFC_1123_DATE_TIME.withZone(ZoneOffset.UTC)

  @volatile private[this] var last: (Long, String) = (0, "")

  def apply(): String = {
    val time = System.currentTimeMillis()
    if (time - last._1 > 1000) {
      last = time -> formatter.format(Instant.ofEpochMilli(time))
    }

    last._2
  }
}