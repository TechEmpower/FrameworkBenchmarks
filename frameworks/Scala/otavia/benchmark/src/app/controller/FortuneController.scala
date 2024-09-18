package app.controller

import app.controller.FortuneController.*
import app.model.Fortune
import cc.otavia.core.actor.{MessageOf, StateActor}
import cc.otavia.core.address.Address
import cc.otavia.core.stack.helper.{FutureState, StartState}
import cc.otavia.core.stack.{AskStack, StackState, StackYield}
import cc.otavia.http.server.{HttpRequest, HttpResponse}
import cc.otavia.sql.statement.PrepareQuery
import cc.otavia.sql.{Connection, RowSet}

import java.util
import java.util.Comparator

class FortuneController extends StateActor[FortuneRequest] {

    private var connection: Address[MessageOf[Connection]] = _
    private val tmpArray: Array[Fortune]                   = new Array[Fortune](13)

    private val comparator = new Comparator[Fortune] {
        override def compare(o1: Fortune, o2: Fortune): Int = o1.message.compareTo(o2.message)
    }

    override protected def afterMount(): Unit = connection = autowire[Connection]()

    //  Test 4: Fortunes
    override protected def resumeAsk(stack: AskStack[FortuneRequest]): StackYield = {
        stack.state match
            case _: StartState =>
                val state = FutureState[RowSet[Fortune]]()
                connection.ask(PrepareQuery.fetchAll[Fortune](SELECT_FORTUNE), state.future)
                stack.suspend(state)
            case state: FutureState[RowSet[Fortune]] =>
                System.arraycopy(state.future.getNow.rows, 0, tmpArray, 0, 12)
                tmpArray(12) = Fortune(0, "Additional fortune added at request time.")
                util.Arrays.sort(tmpArray, comparator)
                val fortunes = tmpArray.clone()
                val response = HttpResponse.builder.setContent(fortunes).build()
                stack.`return`(response)
    }

}

object FortuneController {

    class FortuneRequest extends HttpRequest[Nothing, HttpResponse[Array[Fortune]]]

    private val SELECT_FORTUNE = "SELECT id, message from FORTUNE"

}
