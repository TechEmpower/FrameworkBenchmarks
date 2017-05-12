package server

import java.net.InetSocketAddress

import scala.concurrent.ExecutionContext
import scala.reflect.runtime.universe.Mirror

import com.twitter.finagle.builder.ServerBuilder
import com.twitter.finagle.http.Http
import com.twitter.finagle.http.Request
import com.twitter.finagle.http.RichHttp

import net.fwbrasil.zoot.core.Server
import net.fwbrasil.zoot.core.mapper.StringMapper
import net.fwbrasil.zoot.finagle.{FinagleServer => ZootFinagleServer}
import service.BenchmarkApi
import service.BenchmarkService

class FinagleServer(port: Int)(implicit val executionContext: ExecutionContext, mirror: Mirror, mapper: StringMapper) {

    val serverBuilder =
        ServerBuilder()
            .codec(RichHttp[Request](Http()))
            .keepAlive(true)
            .bindTo(new InetSocketAddress(port))
            .name("BenchmarkFinagleServer")

    val server = Server[BenchmarkApi](new BenchmarkService)

    def start = new ZootFinagleServer(server, serverBuilder.build)
}
