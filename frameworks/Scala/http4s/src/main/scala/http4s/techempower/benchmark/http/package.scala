package http4s.techempower.benchmark

import org.http4s.dsl.impl.QueryParamDecoderMatcher

package object http {

  object IntParamDecoderQueries extends QueryParamDecoderMatcher[Int]("queries")
  object IntParamDecoderUpdates extends QueryParamDecoderMatcher[Int]("updates")

}
