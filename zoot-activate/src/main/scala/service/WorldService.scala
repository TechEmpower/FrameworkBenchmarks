package service

import scala.concurrent.Future

import model.World
import play.api.libs.json.Json
import play.api.libs.json.Json.stringify
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.json.Json.toJson
import play.api.libs.json.Writes

trait WorldService {
    this: BenchmarkService =>

    implicit val worldToJson =
        new Writes[World] {
            def writes(w: World) =
                Json.obj(
                    "id" -> w.id,
                    "randomNumber" -> w.randomNumber)
        }

    def worlds(queries: String) =
        queries match {
            case "" =>
                World.pickOne.map(world => stringify(toJson(world)))
            case queries =>
                execute(parse(queries), World.pickOne)
        }

    def updateWorlds(queries: String) =
        execute(parse(queries), World.updateOne)

    private def execute(queries: Int, future: => Future[World]) =
        Future.sequence(List.fill(queries)(future)).map { worlds =>
            stringify(toJson(worlds))
        }

    private def parse(queries: String) =
        try queries.toInt.max(1).min(500)
        catch {
            case _: Throwable => 1
        }
}
