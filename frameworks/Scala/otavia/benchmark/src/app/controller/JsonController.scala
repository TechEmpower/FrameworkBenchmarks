package app.controller

import app.controller.JsonController.JsonRequest
import app.model.Message
import cc.otavia.core.actor.StateActor
import cc.otavia.http.server.{HttpRequest, HttpResponse}
import cc.otavia.core.stack.{AskStack, StackYield}

class JsonController extends StateActor[JsonRequest] {
    override protected def resumeAsk(stack: AskStack[JsonRequest]): StackYield = {
        stack.`return`(HttpResponse.builder.setContent(Message("Hello, World!")).build())
    }
}
object JsonController {
    object JsonRequest extends HttpRequest[Nothing, HttpResponse[Message]]
    type JsonRequest = JsonRequest.type
}