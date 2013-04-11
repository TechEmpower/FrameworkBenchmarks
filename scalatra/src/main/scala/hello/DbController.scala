package hello

import org.scalatra.ScalatraServlet
import hello.model.{JdbcQuery, World, SingleResultQuery}
import java.util.concurrent.ThreadLocalRandom
import scala.annotation.tailrec

class DbController extends ScalatraServlet with JsonSetup with DbSetup with JndiDataSourceProvider {

  val maxRows = 10000

  val query = SingleResultQuery.byID[World]("SELECT * FROM World WHERE id = ?") {
    rs => World(rs.getInt("id"), rs.getInt("randomNumber"))
  }

  get("/") {
    val count: Int = params.getAs[Int]("queries").getOrElse(1)
    useQuery(query) {
      q =>
        buildResultList(count, q)
    }
  }


  private def buildResultList[T](n: Int, q: JdbcQuery[T]): List[T] = {
    val first = fetchSingle(random, q)
    recursiveFetch(n - 1, q, first, Nil)
  }

  private def fetchSingle[T](id: Int, q: JdbcQuery[T]): T = q.execute(id).getOrElse(null.asInstanceOf[T])

  @tailrec
  private def recursiveFetch[T](n: Int, q: JdbcQuery[T], last: T, fetched: List[T]): List[T] =
    if (n == 0) {
      last :: fetched
    } else {
      recursiveFetch(n - 1, q, fetchSingle(random, q), last :: fetched)
    }

  private def random = ThreadLocalRandom.current().nextInt(maxRows) + 1
}
