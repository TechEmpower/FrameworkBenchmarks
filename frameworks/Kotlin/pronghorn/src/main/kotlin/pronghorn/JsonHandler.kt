package pronghorn

import com.jsoniter.output.EncodingMode
import com.jsoniter.output.JsonStream
import tech.pronghorn.http.*
import tech.pronghorn.http.protocol.CommonContentTypes
import tech.pronghorn.server.handlers.DirectHttpRequestHandler
import java.io.ByteArrayOutputStream

data class JsonExample(val message: String)

class JsonHandler: DirectHttpRequestHandler() {
    companion object {
        init {
            JsonStream.setMode(EncodingMode.DYNAMIC_MODE)
        }
    }

    private val outputStream = ByteArrayOutputStream()
    private val stream = JsonStream(outputStream, 32)

    fun toJson(any: Any): ByteArray{
        outputStream.reset()
        stream.writeVal(any)
        stream.flush()
        return outputStream.toByteArray()
    }

    override fun handleDirect(exchange: HttpExchange): HttpResponse {
        val example = JsonExample("Hello, World!")
        return HttpResponses.OK(toJson(example), CommonContentTypes.ApplicationJson)
    }
}
