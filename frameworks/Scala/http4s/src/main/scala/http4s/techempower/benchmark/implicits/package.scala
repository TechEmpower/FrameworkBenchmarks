package http4s.techempower.benchmark

import cats.Show
import cats.effect.Effect
import model.{Fortune, World}
import org.http4s.EntityEncoder
import org.http4s.circe.jsonEncoderOf
import io.circe.generic.auto._

package object implicits {

  implicit def worldJsonEncoder[F[_]: Effect]: EntityEncoder[F, World] =
    jsonEncoderOf[F, World]

  implicit def worldListJsonEncoder[F[_]: Effect]
    : EntityEncoder[F, List[World]] =
    jsonEncoderOf[F, List[World]]
}
