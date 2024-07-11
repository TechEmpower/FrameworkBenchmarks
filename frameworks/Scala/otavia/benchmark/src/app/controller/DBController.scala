package app.controller

import app.controller.DBController.*
import app.model.World
import cc.otavia.core.actor.{MessageOf, StateActor}
import cc.otavia.core.address.Address
import cc.otavia.core.message.{Ask, Reply}
import cc.otavia.core.stack.helper.{FutureState, FuturesState, StartState}
import cc.otavia.core.stack.{AskStack, StackState, StackYield}
import cc.otavia.http.server.{HttpRequest, HttpResponse}
import cc.otavia.sql.Connection
import cc.otavia.sql.Statement.{ModifyRows, PrepareQuery}

import java.util.SplittableRandom

class DBController extends StateActor[REQ] {

    private var connection: Address[MessageOf[Connection]] = _

    private val random = new SplittableRandom()

    override protected def afterMount(): Unit = connection = autowire[Connection]()

    override protected def resumeAsk(stack: AskStack[REQ & Ask[? <: Reply]]): StackYield =
        stack match
            case stack: AskStack[SingleQueryRequest] if stack.ask.isInstanceOf[SingleQueryRequest] =>
                handleSingleQuery(stack)
            case stack: AskStack[MultipleQueryRequest] if stack.ask.isInstanceOf[MultipleQueryRequest] =>
                handleMultipleQuery(stack)
            case stack: AskStack[UpdateRequest] if stack.ask.isInstanceOf[UpdateRequest] =>
                handleUpdateQuery(stack)

    // Test 2: Single database query
    private def handleSingleQuery(stack: AskStack[SingleQueryRequest]): StackYield = {
        stack.state match
            case _: StartState =>
                val state = FutureState[World]()
                connection.ask(PrepareQuery.fetchOne[World](SELECT_WORLD, Tuple1(randomWorld())), state.future)
                stack.suspend(state)
            case state: FutureState[World] =>
                stack.`return`(state.future.getNow)
    }

    // Test 3: Multiple database queries
    private def handleMultipleQuery(stack: AskStack[MultipleQueryRequest]): StackYield = {
        stack.state match
            case _: StartState =>
                stack.suspend(selectWorlds(normalizeQueries(stack.ask.params)))
            case state: FuturesState[World] =>
                val response = HttpResponse.builder.setContent(state.futures.map(_.getNow)).build()
                stack.`return`(response)
    }

    // Test 5: Database updates
    private def handleUpdateQuery(stack: AskStack[UpdateRequest]): StackYield = {
        stack.state match
            case _: StartState =>
                stack.suspend(selectWorlds(normalizeQueries(stack.ask.params)))
            case state: FuturesState[World] =>
                val worlds = state.futures.map(_.getNow)
                stack.attach(worlds)
                val newState  = FutureState[ModifyRows]()
                val newWorlds = worlds.sortBy(_.id).map(_.copy(randomNumber = randomWorld()))
                connection.ask(PrepareQuery.update(UPDATE_WORLD, newWorlds), newState.future)
                stack.suspend(newState)
            case state: FutureState[ModifyRows] =>
                if (state.future.isFailed) state.future.causeUnsafe.printStackTrace()
                val response = HttpResponse.builder.setContent(stack.attach[Seq[World]]).build()
                stack.`return`(response)
    }

    private def selectWorlds(queries: Int): StackState = {
        val state = FuturesState[World](queries)
        for (future <- state.futures)
            connection.ask(PrepareQuery.fetchOne[World](SELECT_WORLD, Tuple1(randomWorld())), future)
        state
    }

    private def randomWorld(): Int = 1 + random.nextInt(10000)

    private def normalizeQueries(params: Map[String, String]): Int = {
        params.get("queries") match
            case Some(value) =>
                try {
                    val queries = value.toInt
                    if (queries < 1) 1 else if (queries > 500) 500 else queries
                } catch {
                    case e: Throwable => 1
                }
            case None => 1
    }

}

object DBController {

    type REQ = SingleQueryRequest | MultipleQueryRequest | UpdateRequest

    class SingleQueryRequest   extends HttpRequest[Nothing, World]
    class MultipleQueryRequest extends HttpRequest[Nothing, HttpResponse[Seq[World]]]
    class UpdateRequest        extends HttpRequest[Nothing, HttpResponse[Seq[World]]]

    private val SELECT_WORLD = "SELECT id, randomnumber from WORLD where id=$1"
    private val UPDATE_WORLD = "update world set randomnumber=$2 where id=$1"

}
