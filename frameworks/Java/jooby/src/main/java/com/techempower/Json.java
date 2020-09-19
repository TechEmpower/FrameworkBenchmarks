package com.techempower;

import com.dslplatform.json.DslJson;
import com.dslplatform.json.JsonWriter;
import com.dslplatform.json.runtime.Settings;

import java.io.IOException;
import java.nio.ByteBuffer;

public class Json {
  private final static DslJson<Object> dslJson = new DslJson<>(Settings.basicSetup());

  private final static ThreadLocal<JsonWriter> pool = new ThreadLocal<JsonWriter>() {
    @Override protected JsonWriter initialValue() {
      return dslJson.newWriter(1024);
    }
  };

  public static final ByteBuffer encode(Object data) throws IOException {
    JsonWriter writer = pool.get();
    writer.reset();
    dslJson.serialize(writer, data);
    return ByteBuffer.wrap(writer.getByteBuffer(), 0, writer.size());
  }

}
