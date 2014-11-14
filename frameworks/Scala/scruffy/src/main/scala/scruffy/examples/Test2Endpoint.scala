package scruffy.examples

import com.mongodb.casbah.Imports._
import com.sksamuel.scruffy.EndpointProvider

/** @author Stephen Samuel */
class Test2Endpoint() extends EndpointProvider {
  val hostname = "database_host"
  val connection = MongoConnection(hostname, 27017)
  val collection = connection.getDB("hello_world").getCollection("world")

  val random = new scala.util.Random(System.currentTimeMillis)
  val fields = DBObject("_id" -> true, "randomNumber" -> true)

  //uncomment to populate
  //for ( k <- 1 to 10000 )
  //  collection.save(DBObject("_id" -> k, "id" -> k, "randomNumber" -> random.nextInt(10000).toDouble))

  get("db").json {
    req =>
      val id = random.nextInt(10000)
      val dbo = collection.findOne(DBObject("_id" -> id), fields)
      val randomNumber = Math.round(dbo.get("randomNumber").toString.toFloat)
      Output(id, randomNumber)
  }
}

case class Output(id: Int, randomNumber: Int)
