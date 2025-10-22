package ba.sake.sharaf.benchmark

import ba.sake.sharaf.*, routing.*
import ba.sake.querson.*
import ba.sake.tupson.*

class BenchmarkRoutes(dao: DAO) {

  def routes: Routes = Routes {
    case GET() -> Path("plaintext") =>
      Response.withBody("Hello, World!").settingHeader("Server", "sharaf")

    case GET() -> Path("json") =>
      Response
        .withBody(MessageResponse("Hello, World!"))
        .settingHeader("Server", "sharaf")

    case GET() -> Path("db") =>
      val row = dao.getRandomWorld()
      val body = WorldResponse(row.id, row.randomnumber)
      Response.withBody(body).settingHeader("Server", "sharaf")

    case GET() -> Path("queries") =>
      val queriesCountStr = Request.current.queryParams[QueriesQP].queries
      var queriesCount = queriesCountStr.toIntOption.getOrElse(1)
      if queriesCount < 1 then queriesCount = 1
      if queriesCount > 500 then queriesCount = 500
      val rows = dao.getRandomWorlds(queriesCount)
      val body = rows.map(row => WorldResponse(row.id, row.randomnumber))
      Response.withBody(body).settingHeader("Server", "sharaf")

    case GET() -> Path("fortunes") =>
      val rows = dao
        .getFortunes()
        .appended(FortuneRow(0, "Additional fortune added at request time."))
      val rowsSorted = rows.sortBy(_.message)
      val body = FortunesPage(rowsSorted)
      Response.withBody(body).settingHeader("Server", "sharaf")

    case GET() -> Path("updates") =>
      val queriesCountStr = Request.current.queryParams[QueriesQP].queries
      var queriesCount = queriesCountStr.toIntOption.getOrElse(1)
      if queriesCount < 1 then queriesCount = 1
      if queriesCount > 500 then queriesCount = 500
      val rows = dao.getRandomWorlds(queriesCount)
      val updatedRows = rows.map(_.copy(randomnumber = dao.getRandomRowId()))
      dao.updateWorlds(updatedRows)
      val body = updatedRows.map(row => WorldResponse(row.id, row.randomnumber))
      Response.withBody(body).settingHeader("Server", "sharaf")

  }
}

// query params
case class QueriesQP(queries: String = "1") derives QueryStringRW

// json responses
case class MessageResponse(message: String) derives JsonRW
case class WorldResponse(id: Int, randomNumber: Int) derives JsonRW
