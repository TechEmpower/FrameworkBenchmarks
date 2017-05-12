package service

import scala.concurrent.ExecutionContext

class BenchmarkService(implicit val executionContext: ExecutionContext)
    extends BenchmarkApi
    with WorldService
    with FortunesService 