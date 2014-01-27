package controllers

import scala.collection.mutable.StringBuilder
import scala.slick.jdbc.{ StaticQuery => Q }
import java.util.concurrent.ThreadLocalRandom
import org.cognition._
import org.cognition.implicits.Numeric._
import conf.Authentication._
import models._
import utils.Common._
import sqltyped._
import shapeless.record.{ recordOps => r }

class DbController extends Controller {

  private[this] final val maxId = 9999

  get("db") {
    withJDBC { implicit request => session =>
      implicit val conn = session.conn
      val random = ThreadLocalRandom.current()
      val world = sql("select id, randomNumber from world where id = ?")
        .apply(random.nextInt(maxId) + 1).map(r(_))
        .map(x => new World(x("id").toInt, x("randomNumber"))).head

      render.json(world)
    }
  }

  get("queries") {
    withJDBC { implicit request => session =>
      implicit val conn = session.conn
      val random = ThreadLocalRandom.current()
      val count = normalizeInt(params("count", "1").toIntOption.getOrElse(1), 1, 500)
      val buf = Array.ofDim[World](count)
      (0 until count).foreach { i =>
        buf(i) = sql("select id, randomNumber from world where id = ?")
          .apply(random.nextInt(maxId) + 1).map(r(_))
          .map(x => new World(x("id").toInt, x("randomNumber"))).head
      }

      render.json(buf)
    }
  }

  get("update") {
    withJDBC { implicit request => implicit session =>
      implicit val conn = session.conn
      val random = ThreadLocalRandom.current()
      val count = normalizeInt(params("count", "1").toIntOption.getOrElse(1), 1, 500)
      val buf = Array.ofDim[World](count)

      val updates = new StringBuilder
      updates.append("update world set randomNumber = case ")

      (0 until count).foreach { i =>
        val world = sql("select id, randomNumber from world where id = ?")
          .apply(random.nextInt(maxId) + 1).map(r(_))
          .map(x => new World(x("id").toInt, x("randomNumber"))).head
        world.randomNumber = random.nextInt(maxId) + 1
        updates.append(s"when id = ${world.id} then ${world.randomNumber} ")
        buf(i) = world
      }

      updates.append("end where id in (" + buf.map(_.id).mkString(",") + ")")
      Q.updateNA(updates.result).execute
      render.json(buf)
    }
  }

}

