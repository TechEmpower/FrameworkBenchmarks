package http4s.techempower.benchmark

import cats.effect.Effect
import cats.implicits._

package object syntax {

  implicit class WithFilterSyntaxEff[F[_]: Effect, A](val fa: F[A]) {
    def withFilter(p: A => Boolean): F[A] =
      fa >>= (a =>
        if (p(a)) fa
        else
          Effect[F].raiseError[A](
            new IllegalStateException("invalid filter state")
          ))
  }
}
