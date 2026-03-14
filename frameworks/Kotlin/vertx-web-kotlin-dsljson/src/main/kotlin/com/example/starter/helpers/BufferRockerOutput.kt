package com.example.starter.helpers

import com.fizzed.rocker.ContentType
import com.fizzed.rocker.RockerOutput
import com.fizzed.rocker.RockerOutputFactory
import io.netty.buffer.ByteBuf
import io.vertx.core.buffer.Buffer
import io.vertx.core.impl.buffer.VertxByteBufAllocator
import io.vertx.core.internal.buffer.BufferInternal
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets

class BufferRockerOutput private constructor(
    private val contentType: ContentType
) : RockerOutput<BufferRockerOutput> {

    private val byteBuf: ByteBuf = VertxByteBufAllocator.DEFAULT.directBuffer()
    private val buffer: Buffer = BufferInternal.buffer(byteBuf)

    private fun reset() {
        byteBuf.resetReaderIndex()
        byteBuf.resetWriterIndex()
    }

    override fun w(bytes: ByteArray): BufferRockerOutput {
        buffer.appendBytes(bytes)
        return this
    }

    override fun w(s: String): BufferRockerOutput {
        buffer.appendString(s)
        return this
    }

    override fun getContentType(): ContentType = contentType

    override fun getCharset(): Charset = StandardCharsets.UTF_8

    override fun getByteLength(): Int = buffer.length()

    fun buffer(): Buffer = buffer

    companion object {
        fun factory(contentType: ContentType): RockerOutputFactory<BufferRockerOutput> {
            val output = BufferRockerOutput(contentType)
            return RockerOutputFactory { _, _ ->
                output.reset()
                output
            }
        }
    }
}