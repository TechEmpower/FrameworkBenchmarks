package com.techempower;

import java.nio.ByteBuffer;
import java.util.List;

import com.dslplatform.json.DslJson;
import com.dslplatform.json.JsonWriter;
import com.dslplatform.json.runtime.Settings;
import io.jooby.output.Output;
import io.jooby.output.OutputFactory;

public class Json {
  private final static DslJson<Object> dslJson = new DslJson<>(Settings.basicSetup());

  private final static ThreadLocal<JsonWriter> pool = new ThreadLocal<JsonWriter>() {
    @Override protected JsonWriter initialValue() {
      return dslJson.newWriter(1024);
    }
  };
  private static OutputFactory outputFactory;

  public static void configure(OutputFactory outputFactory) {
    Json.outputFactory =outputFactory.getContextFactory();
  }

  public static Output encode(Message data) {
    JsonWriter writer = pool.get();
    writer.reset();
    var converter = new _Message_DslJsonConverter.ObjectFormatConverter(dslJson);
    converter.write(writer, data);
    return outputFactory.wrap(writer.getByteBuffer(), 0, writer.size());
  }

  public static Output encode(World data) {
    JsonWriter writer = pool.get();
    writer.reset();
    var converter = new _World_DslJsonConverter.ObjectFormatConverter(dslJson);
    converter.write(writer, data);
    return outputFactory.wrap(writer.getByteBuffer(), 0, writer.size());
  }

  public static Output encode(World[] data) {
    JsonWriter writer = pool.get();
    writer.reset();
    var converter = new _World_DslJsonConverter.ObjectFormatConverter(dslJson);
    writer.serialize(data, converter);
    return outputFactory.wrap(writer.getByteBuffer(), 0, writer.size());
  }

  public static Output encode(List<World> data) {
    JsonWriter writer = pool.get();
    writer.reset();
    var converter = new _World_DslJsonConverter.ObjectFormatConverter(dslJson);
    writer.serialize(data, converter);
    return outputFactory.wrap(writer.getByteBuffer(), 0, writer.size());
  }
}
