package app

import app.controller.DBController.*
import app.controller.FortuneController.*
import app.controller.{DBController, FortuneController}
import app.model.*
import app.util.FortunesRender
import cc.otavia.core.actor.ChannelsActor.{Bind, ChannelEstablished}
import cc.otavia.core.actor.MainActor
import cc.otavia.core.slf4a.LoggerFactory
import cc.otavia.core.stack.helper.{FutureState, StartState}
import cc.otavia.core.stack.{NoticeStack, StackYield}
import cc.otavia.core.system.ActorSystem
import cc.otavia.http.HttpMethod.*
import cc.otavia.http.MediaType
import cc.otavia.http.MediaType.*
import cc.otavia.http.server.*
import cc.otavia.http.server.Router.*
import cc.otavia.json.JsonSerde
import cc.otavia.serde.helper.BytesSerde
import cc.otavia.sql.Connection

import java.io.File
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path

private class ServerMain(val port: Int = 8080) extends MainActor(Array.empty) {

    override def main0(stack: NoticeStack[MainActor.Args]): StackYield = stack.state match
        case _: StartState =>
            val worldResponseSerde    = HttpResponseSerde.json(summon[JsonSerde[World]])
            val worldsResponseSerde   = HttpResponseSerde.json(JsonSerde.derived[Seq[World]])
            val fortunesResponseSerde = HttpResponseSerde(new FortunesRender(), MediaType.TEXT_HTML_UTF8)

            val dbController      = autowire[DBController]()
            val fortuneController = autowire[FortuneController]()

            val routers = Seq(
              // Test 6: plaintext
              constant[Array[Byte]](GET, "/plaintext", "Hello, World!".getBytes(UTF_8), BytesSerde, TEXT_PLAIN_UTF8),
              // Test 1: JSON serialization
              constant[Message](GET, "/json", Message("Hello, World!"), summon[JsonSerde[Message]], APP_JSON),
              // Test 2: Single database query.
              get("/db", dbController, () => new SingleQueryRequest(), worldResponseSerde),
              // Test 3: Multiple database queries
              get("/queries", dbController, () => new MultipleQueryRequest(), worldsResponseSerde),
              // Test 5: Database updates
              get("/updates", dbController, () => new UpdateRequest(), worldsResponseSerde),
              //  Test 4: Fortunes
              get("/fortunes", fortuneController, () => new FortuneRequest(), fortunesResponseSerde)
            )
            val server = system.buildActor(() => new HttpServer(system.actorWorkerSize, routers))
            val state  = FutureState[ChannelEstablished]()
            server.ask(Bind(port), state.future)
            stack.suspend(state)
        case state: FutureState[ChannelEstablished] =>
            if (state.future.isFailed) state.future.causeUnsafe.printStackTrace()
            logger.info(s"http server bind port $port success")
            stack.`return`()

}

@main def startup(url: String, user: String, password: String, poolSize: Int): Unit =
    val system = ActorSystem()
    val logger = LoggerFactory.getLogger("startup", system)
    logger.info("starting http server")
    system.buildActor(() => new Connection(url, user, password), global = true, num = poolSize)
    system.buildActor(() => new DBController(), global = true, num = system.actorWorkerSize)
    system.buildActor(() => new FortuneController(), global = true, num = system.actorWorkerSize)
    system.buildActor(() => new ServerMain())
