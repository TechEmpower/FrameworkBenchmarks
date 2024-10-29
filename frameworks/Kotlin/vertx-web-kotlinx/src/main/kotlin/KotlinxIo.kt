import io.vertx.core.streams.WriteStream
import io.vertx.kotlin.coroutines.coAwait
import kotlinx.coroutines.runBlocking
import kotlinx.io.RawSink
import kotlinx.io.readByteArray
import io.vertx.core.buffer.Buffer as VertxBuffer
import kotlinx.io.Buffer as KotlinxIoBuffer

@Suppress("NOTHING_TO_INLINE")
private inline fun Long.toIntOrThrow(): Int {
    require(this in Int.MIN_VALUE.toLong()..Int.MAX_VALUE.toLong())
    return toInt()
}

@JvmInline
value class VertxBufferWriteStreamSink(val writeStream: WriteStream<VertxBuffer>) : RawSink {
    override fun write(source: KotlinxIoBuffer, byteCount: Long) {
        runBlocking {
            // `source` is temporarily converted to a byte array because wrapping it as a Vert.x `Buffer` is too complicated to implement.
            writeStream.write(VertxBuffer.buffer(source.readByteArray(byteCount.toIntOrThrow()))).coAwait()
        }
    }

    override fun flush() {}

    override fun close() {
        writeStream.end()
    }
}

// not used currently
fun WriteStream<VertxBuffer>.toRawSink(): RawSink =
    VertxBufferWriteStreamSink(this)


@JvmInline
value class VertxBufferSink(val vertxBuffer: VertxBuffer) : RawSink {
    override fun write(source: KotlinxIoBuffer, byteCount: Long) {
        // same problem as above
        vertxBuffer.appendBytes(source.readByteArray(byteCount.toIntOrThrow()))
    }

    override fun flush() {}

    override fun close() {}
}

fun VertxBuffer.toRawSink(): RawSink =
    VertxBufferSink(this)
