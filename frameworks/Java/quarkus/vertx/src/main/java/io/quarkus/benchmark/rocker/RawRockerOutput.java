package io.quarkus.benchmark.rocker;

import com.fizzed.rocker.ContentType;
import com.fizzed.rocker.RockerOutputFactory;
import io.netty.buffer.ByteBuf;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.buffer.impl.PartialPooledByteBufAllocator;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

final class RawRockerOutput implements VertxRawRockerOutput {

    private final ByteBuf buff = PartialPooledByteBufAllocator.INSTANCE.directBuffer();
    private final Buffer buffer = Buffer.buffer(buff);

    RawRockerOutput() {
    }

    public static RockerOutputFactory<VertxRawRockerOutput> raw() {
        final RawRockerOutput output = new RawRockerOutput();
        return (_contentType, charsetName) -> {
            output.reset();
            return output;
        };
    }

    private void reset() {
        buff.resetReaderIndex();
        buff.resetWriterIndex();
    }

    @Override
    public RawRockerOutput w(final byte[] bytes) throws IOException {
        buffer.appendBytes(bytes);
        return this;
    }

    @Override
    public RawRockerOutput w(final String s) throws IOException {
        buffer.appendString(s);
        return this;
    }

    @Override
    public ContentType getContentType() {
        return ContentType.RAW;
    }

    @Override
    public Charset getCharset() {
        return StandardCharsets.UTF_8;
    }

    @Override
    public int getByteLength() {
        return buffer.length();
    }

    @Override
    public Buffer buffer() {
        return buffer;
    }
}
