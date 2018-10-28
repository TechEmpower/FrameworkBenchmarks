package pronghorn.handlers

import com.jsoniter.output.JsonStream
import pronghorn.utils.JsonSupport
import tech.pronghorn.http.*
import tech.pronghorn.http.protocol.CommonContentTypes
import tech.pronghorn.server.requesthandlers.DirectHttpRequestHandler
import java.io.ByteArrayOutputStream

data class JsonExample(val message: String)

class JsonHandler: DirectHttpRequestHandler(), JsonSupport {
    override val outputStream = ByteArrayOutputStream()
    override val stream = JsonStream(outputStream, 32)

    override fun handleDirect(exchange: HttpExchange): HttpResponse {
        val example = JsonExample("Hello, World!")
        return HttpResponses.OK(toJson(example), CommonContentTypes.ApplicationJson)
    }
}
