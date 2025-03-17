package com.techempower.rocker;

import com.fizzed.rocker.ContentType;
import com.fizzed.rocker.RockerOutput;
import com.fizzed.rocker.RockerOutputFactory;

import java.io.IOException;
import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

public class BufferRockerOutput implements RockerOutput<BufferRockerOutput> {
    private final ByteBuffer buffer;

    public BufferRockerOutput(ByteBuffer buffer) {
        this.buffer = buffer;
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
    public BufferRockerOutput w(String string) throws IOException {
        buffer.put(string.getBytes(getCharset()));
        return this;
    }

    @Override
    public BufferRockerOutput w(byte[] bytes) throws IOException {
        buffer.put(bytes);
        return this;
    }

    @Override
    public int getByteLength() {
        return buffer.remaining();
    }

    public ByteBuffer toBuffer() {
        return buffer.flip();
    }

    public static RockerOutputFactory<BufferRockerOutput> factory() {
        var cache = new ThreadLocal<BufferRockerOutput>() {
            @Override
            protected BufferRockerOutput initialValue() {
                return new BufferRockerOutput(ByteBuffer.allocateDirect(2048));
            }
        };
        return (contentType, charsetName) -> cache.get().reset();
    }

    private BufferRockerOutput reset() {
        buffer.clear();
        return this;
    }
}
