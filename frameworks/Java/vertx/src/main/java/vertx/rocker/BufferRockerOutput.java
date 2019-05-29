package vertx.rocker;

import com.fizzed.rocker.ContentType;
import com.fizzed.rocker.RockerOutput;
import com.fizzed.rocker.RockerOutputFactory;
import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.net.impl.PartialPooledByteBufAllocator;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

public class BufferRockerOutput implements RockerOutput<BufferRockerOutput> {

  public static RockerOutputFactory<BufferRockerOutput> factory(ContentType contentType) {
    BufferRockerOutput output = new BufferRockerOutput(contentType);
    return (_contentType, charsetName) -> {
      output.reset();
      return output;
    };
  }

  private final ByteBuf buff = PartialPooledByteBufAllocator.UNPOOLED.directBuffer();
  private final Buffer buffer = Buffer.buffer(buff);
  private final ContentType contentType;

  BufferRockerOutput(ContentType contentType) {
    this.contentType = contentType;
  }

  private void reset() {
    buff.resetReaderIndex();
    buff.resetWriterIndex();
  }

  @Override
  public BufferRockerOutput w(byte[] bytes) throws IOException {
    buffer.appendBytes(bytes);
    return this;
  }

  @Override
  public BufferRockerOutput w(String s) throws IOException {
    buffer.appendString(s);
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
    return buffer.length();
  }

  public Buffer buffer() {
    return buffer;
  }
}
