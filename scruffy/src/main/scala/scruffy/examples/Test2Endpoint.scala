package scruffy.examples

import com.mongodb.casbah.Imports._
import com.sksamuel.scruffy.EndpointProvider

/** @author Stephen Samuel */
class Test2Endpoint(hostname: String) extends EndpointProvider {

  val connection = MongoConnection(hostname, 27017)
  val collection = connection.getDB("world").getCollection("world")

  val random = new scala.util.Random(System.currentTimeMillis)
  val fields = DBObject("_id" -> true, "randomNumber" -> true)

  // uncomment to populate
  // for ( k <- 1 to 10000 )
  //   collection.save(DBObject("_id" -> k, "id" -> k, "randomNumber" -> random.nextInt(10000)))

  get("db").json {
    req =>
      val id = random.nextInt(10000)
      collection.findOne(DBObject("_id" -> id), fields)
  }
}


