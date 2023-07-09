package io.quarkus.benchmark.utils.rocker;

import com.fizzed.rocker.ContentType;
import com.fizzed.rocker.RockerOutput;
import com.fizzed.rocker.RockerOutputFactory;
import io.netty.buffer.ByteBuf;
import io.netty.util.concurrent.FastThreadLocal;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.buffer.impl.BufferImpl;
import io.vertx.core.buffer.impl.VertxByteBufAllocator;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

public class HtmlUtf8BufferRockerOutput implements RockerOutput<HtmlUtf8BufferRockerOutput> {

    private static final FastThreadLocal<HtmlUtf8BufferRockerOutput> SCRATCH_ROCKER_OUTPUT = new FastThreadLocal<>() {
        @Override
        protected HtmlUtf8BufferRockerOutput initialValue() {
            return new HtmlUtf8BufferRockerOutput(ContentType.HTML);
        }

        @Override
        protected void onRemoval(final HtmlUtf8BufferRockerOutput value) {
            value.buff.release();
        }
    };

    public static RockerOutputFactory<HtmlUtf8BufferRockerOutput> threadLocalFactory() {
        return (_contentType, charsetName) -> SCRATCH_ROCKER_OUTPUT.get().reset();
    }

    private final ByteBuf buff = VertxByteBufAllocator.DEFAULT.heapBuffer();
    private final Buffer vertxBuff = BufferImpl.buffer(buff);
    private final ContentType contentType;

    HtmlUtf8BufferRockerOutput(ContentType contentType) {
        this.contentType = contentType;
    }

    @Override
    public HtmlUtf8BufferRockerOutput w(byte[] bytes) {
        buff.writeBytes(bytes);
        return this;
    }

    @Override
    public HtmlUtf8BufferRockerOutput w(String s) {
        // UGLY!!! we trust vectorized & informed intrinsics for Strings here!
        buff.writeBytes(s.getBytes(StandardCharsets.UTF_8));
        return this;
    }

    @Override
    public ContentType getContentType() {
        return contentType;
    }

    @Override
    public Charset getCharset() {
        return StandardCharsets.UTF_8;
    }

    @Override
    public int getByteLength() {
        return buff.readableBytes();
    }

    public Buffer buffer() {
        return vertxBuff;
    }

    public HtmlUtf8BufferRockerOutput reset() {
        buff.clear();
        return this;
    }

}
