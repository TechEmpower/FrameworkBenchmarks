package scruffy.examples

import java.util.concurrent.ThreadLocalRandom

import com.mongodb.casbah.Imports._
import com.sksamuel.scruffy.HttpModule

/** @author Stephen Samuel */
object Test2Endpoint extends HttpModule {

  import com.sksamuel.scruffy.jackson.ScruffyJackson.Implicits._

  val hostname = "127.0.0.1"
  val connection = MongoConnection(hostname, 27017)
  val collection = connection.getDB("hello_world").getCollection("world")

  val fields = DBObject("_id" -> true, "randomNumber" -> true)

  //uncomment to populate
  //for ( k <- 1 to 10000 )
  //  collection.save(DBObject("_id" -> k, "id" -> k, "randomNumber" -> random.nextInt(10000).toDouble))

  get("db") { req =>
    val id = ThreadLocalRandom.current.nextInt(10000)
    val dbo = collection.findOne(DBObject("_id" -> id), fields)
    val randomNumber = Math.round(dbo.get("randomNumber").toString.toFloat)
    Output(id, randomNumber).json
  }
}

case class Output(id: Int, randomNumber: Int)
