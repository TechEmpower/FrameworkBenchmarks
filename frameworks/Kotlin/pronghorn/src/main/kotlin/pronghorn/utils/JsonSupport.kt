package pronghorn.utils

import com.jsoniter.output.JsonStream
import java.io.ByteArrayOutputStream

interface JsonSupport {
    val outputStream: ByteArrayOutputStream
    val stream: JsonStream

    fun toJson(any: Any): ByteArray {
        outputStream.reset()
        stream.writeVal(any)
        stream.flush()
        return outputStream.toByteArray()
    }
}
