import cats.Show
import model.{Fortune, World}

package object implicits {

  implicit val worldShow: Show[World] =
    (t: World) => s"""{"id":"${t.id}","randomNumber":"${t.randomNumber}"}"""

  implicit val fortuneShow: Show[Fortune] =
    (t: Fortune) => s"""{"id":"${t.id}","randomNumber":"${t.message}"}"""
}
