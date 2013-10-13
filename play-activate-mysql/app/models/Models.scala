package models

import persistenceContext._
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.json.Writes

class ActivateFortune(val legacyId: Long, val message: String) extends Entity

object ActivateFortune {
    def all =
        cachedQuery {
            (fortune: ActivateFortune) => where()
        }
}

class ActivateWorld(val legacyId: Long, var randomNumber: Long) extends Entity

object ActivateWorld {
    def fingByLegacyId(legacyId: Long) = {
        val lazyList = indexWorldByLegacyId.get(legacyId)
        lazyList.headOption.getOrElse(throw new IllegalStateException("invalid id " + legacyId))
    }
}

object Models {

    implicit val worldToJson =
        new Writes[ActivateWorld] {
            def writes(w: ActivateWorld): JsValue =
                Json.obj(
                    "id" -> w.legacyId,
                    "randomNumber" -> w.randomNumber)
        }

}