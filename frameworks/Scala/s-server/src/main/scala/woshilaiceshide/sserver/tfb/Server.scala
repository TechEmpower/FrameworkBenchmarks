package woshilaiceshide.sserver.tfb

import woshilaiceshide.sserver.http._
import woshilaiceshide.sserver.nio._
import spray.http._
import spray.http.HttpEntity.apply
import spray.http.StatusCode.int2StatusCode

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala._

object Server extends App {

  val log = org.slf4j.LoggerFactory.getLogger(Server.getClass);

  val mapper = new ObjectMapper()
  mapper.registerModule(DefaultScalaModule)
  final case class Message(message: String)

  import scala.concurrent._
  val executor = java.util.concurrent.Executors.newSingleThreadExecutor()
  implicit val ec = ExecutionContext.fromExecutor(executor)

  val handler = new HttpChannelHandler {

    private val ping = new HttpResponse(200, HttpEntity(ContentTypes.`text/plain`, "Hello World"))
    private def write_ping(channel: HttpChannel) = {
      channel.writeResponse(ping)
      ResponseAction.responseNormally
    }
    private val path_ping = Uri.Path("/ping")

    private val phelloworld = new HttpResponse(200, HttpEntity(ContentTypes.`text/plain`, "Hello, World!"))
    private def write_phelloworld(channel: HttpChannel) = {
      channel.writeResponse(phelloworld)
      ResponseAction.responseNormally
    }
    private val path_plaintext = Uri.Path("/plaintext")

    //JsObject("message" -> JsString("Hello, World!"))
    private val path_json = Uri.Path("/json")
    private def jhelloworld = new HttpResponse(200, HttpEntity(ContentTypes.`application/json`, mapper.writeValueAsBytes(Message("Hello, World!"))))
    private def write_jhelloworld(channel: HttpChannel) = {
      channel.writeResponse(jhelloworld)
      ResponseAction.responseNormally
    }

    private def write_404(channel: HttpChannel) = {
      channel.writeResponse { new HttpResponse(404) }
      ResponseAction.responseNormally
    }

    private def write_400(channel: HttpChannel) = {
      channel.writeResponse { new HttpResponse(400) }
      ResponseAction.responseNormally
    }

    private val ping_asynchronously = new HttpResponse(200, HttpEntity(ContentTypes.`text/plain`, "Hello World Asynchronously"))
    private def write_ping_asynchronously(channel: HttpChannel) = {
      //channel.post_to_io_thread { channel.writeResponse(ping_asynchronously) }
      Future { channel.writeResponse(ping_asynchronously) }
      ResponseAction.responseNormally
    }

    def requestReceived(request: HttpRequest, channel: HttpChannel, classifier: RequestClassifier): ResponseAction = request match {

      case HttpRequest(HttpMethods.GET, uri, _, _, _) if uri.path == path_json => write_jhelloworld(channel)

      case HttpRequest(HttpMethods.GET, uri, _, _, _) if uri.path == path_plaintext => write_phelloworld(channel)

      case HttpRequest(HttpMethods.GET, uri, _, _, _) if uri.path == path_ping => write_ping(channel)

      case HttpRequest(HttpMethods.GET, Uri.Path("/ping_asynchronously"), _, _, _) => write_ping_asynchronously(channel)

      case _: HttpRequest => write_404(channel)
    }

  }

  val http_configurator = new HttpConfigurator(max_request_in_pipeline = 8, use_direct_byte_buffer_for_cached_bytes_rendering = false)

  val factory = new HttpChannelHandlerFactory(handler, http_configurator)

  val listening_channel_configurator: ServerSocketChannelWrapper => Unit = wrapper => {
    wrapper.setOption[java.lang.Boolean](java.net.StandardSocketOptions.SO_REUSEADDR, true)
    wrapper.setBacklog(1024 * 8)
  }

  val accepted_channel_configurator: SocketChannelWrapper => Unit = wrapper => {
    wrapper.setOption[java.lang.Boolean](java.net.StandardSocketOptions.TCP_NODELAY, true)
  }

  /*
  val threadFactory = new java.util.concurrent.ThreadFactory() {
    def newThread(r: Runnable) = {
      new Thread(r)
    }
  }
  val mt = new MultipleThreadHandlerFactory(1, threadFactory, Integer.MAX_VALUE, factory)
  */

  val configurator = XNioConfigurator(count_for_reader_writers = Runtime.getRuntime().availableProcessors() - 1, //2,
    listening_channel_configurator = listening_channel_configurator,
    accepted_channel_configurator = accepted_channel_configurator,
    //buffer_pool_factory = DefaultByteBufferPoolFactory(1, 1, true),
    //buffer_pool_factory = DefaultByteBufferPoolFactory(512, 64, true),
    //more i/o, more asynchronously, then make it bigger
    buffer_pool_factory = DefaultByteBufferPoolFactory(512, 64, true),
    io_thread_factory = new woshilaiceshide.sserver.http.AuxThreadFactory())

  val port = 8080

  val server = NioSocketServer(
    "0.0.0.0",
    port,
    factory,
    configurator)

  server.register_on_termination {
    executor.shutdown()
  }

  server.start(false)

}