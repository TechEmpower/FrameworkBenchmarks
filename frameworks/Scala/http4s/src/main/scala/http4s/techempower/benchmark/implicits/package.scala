package http4s.techempower.benchmark

import cats.Show
import cats.effect.Effect
import model.{Fortune, World}
import org.http4s.EntityEncoder
import org.http4s.circe.jsonEncoderOf
import io.circe.generic.auto._

package object implicits {

  implicit val worldShow: Show[World] =
    (t: World) => s"""{"id":"${t.id}","randomNumber":"${t.randomNumber}"}"""

  implicit val fortuneShow: Show[Fortune] =
    (t: Fortune) => s"""{"id":"${t.id}","randomNumber":"${t.message}"}"""

  implicit def worldListShow(implicit S: Show[World]): Show[List[World]] =
    (t: List[World]) => "[" + t.foldLeft("]")((a, b) => S.show(b) + "," + a)

  implicit def worldJsonEncoder[F[_]: Effect]: EntityEncoder[F, World] =
    jsonEncoderOf[F, World]

  implicit def worldListJsonEncoder[F[_]: Effect]
    : EntityEncoder[F, List[World]] =
    jsonEncoderOf[F, List[World]]
}
