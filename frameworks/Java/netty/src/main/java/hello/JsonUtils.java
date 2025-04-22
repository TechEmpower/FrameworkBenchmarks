package hello;

import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.VarHandle;
import java.util.Arrays;

import com.jsoniter.output.JsonStream;
import com.jsoniter.output.JsonStreamPool;
import com.jsoniter.spi.Config;
import com.jsoniter.spi.JsonException;

import io.netty.util.concurrent.FastThreadLocal;

public class JsonUtils {

   private static final VarHandle INDENTATION;

   static {
      try {
         var lookup = MethodHandles.privateLookupIn(JsonStream.class, MethodHandles.lookup());
         INDENTATION = lookup.findVarHandle(JsonStream.class, "indention", int.class);
         var dummy = new JsonStream(null, 32);
         INDENTATION.set(dummy, 4);
      } catch (Exception e) {
         throw new RuntimeException(e);
      }
   }

   private static void setIndentation(JsonStream stream, int value) {
      INDENTATION.set(stream, value); // Plain store
   }

   private static final FastThreadLocal<JsonStream> JSON_STREAM = new FastThreadLocal<>();

   public static JsonStream acquireJsonStreamFromEventLoop() {
      var stream = JSON_STREAM.get();
      if (stream == null) {
         stream = new JsonStream(null, 512) {
            // this is to save virtual threads to use thread locals
            @Override
            public Config currentConfig() {
               return Config.INSTANCE;
            }
         };
      } else {
         stream.reset(null);
         JSON_STREAM.set(null);
      }
      return stream;
   }

   public static JsonStream releaseJsonStreamFromEventLoop(JsonStream jsonStream) {
      jsonStream.configCache = null;
      setIndentation(jsonStream, 0);
      JSON_STREAM.set(jsonStream);
      return jsonStream;
   }

   public static byte[] serializeMsg(Message message) {
      var stream = JsonStreamPool.borrowJsonStream();
      try {
         return serializeMsg(message, stream);
      } finally {
         // Reset the stream to avoid memory leaks
         JsonStreamPool.returnJsonStream(stream);
      }
   }

   public static byte[] serializeMsg(Message obj, JsonStream stream) {
      try {
         stream.writeVal(Message.class, obj);
         return Arrays.copyOfRange(stream.buffer().data(), 0, stream.buffer().tail());
      } catch (IOException e) {
         throw new JsonException(e);
      }
   }
}
