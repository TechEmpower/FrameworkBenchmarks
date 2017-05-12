package service

import model.persistenceContext._
import java.io.InputStreamReader
import java.io.StringWriter
import scala.collection.JavaConversions.seqAsJavaList
import com.github.mustachejava.DefaultMustacheFactory
import model.Fortune
import net.fwbrasil.zoot.core.response.Response
import net.fwbrasil.radon.transaction.Transaction

trait FortunesService {
    this: BenchmarkService =>

    val mustache = {
        val mustacheFactory = new DefaultMustacheFactory
        val resource = new InputStreamReader(getClass.getResourceAsStream("fortunes.mustache"))
        mustacheFactory.compile(resource, "fortunes")
    }

    def fortunes =
        Fortune.all.map { all =>
            val writer = new StringWriter
            transactional(new Transaction) {
                val fortunes = new Fortune(0, "Additional fortune added at request time.") +: all
                mustache.execute(writer, fortunes.sortBy(_.message): java.util.List[Fortune])
                writer.toString
            }
        }
}