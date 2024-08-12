package io.helidon.benchmark.nima;

import java.io.IOException;
import java.util.Arrays;
import java.util.Map;
import java.util.List;

import com.jsoniter.output.JsonStream;
import com.jsoniter.output.JsonStreamPool;
import com.jsoniter.spi.JsonException;

public class JsonSerializer {

    private JsonSerializer() {
    }

    /**
     * Serialize an instance into a JSON object and return it as a byte array.
     *
     * @param obj the instance
     * @return the byte array
     */
    public static byte[] serialize(Object obj) {
        JsonStream stream = JsonStreamPool.borrowJsonStream();
        try {
            stream.reset(null);
            stream.writeVal(obj.getClass(), obj);
            return Arrays.copyOfRange(stream.buffer().data(), 0, stream.buffer().tail());
        } catch (IOException e) {
            throw new JsonException(e);
        } finally {
            JsonStreamPool.returnJsonStream(stream);
        }
    }

    /**
     * Serialize a map of strings into a JSON object and return it as a byte array.
     *
     * @param map the map
     * @return the byte array
     */
    public static byte[] serialize(Map<String, String> map) {
        JsonStream stream = JsonStreamPool.borrowJsonStream();
        try {
            stream.reset(null);
            stream.writeObjectStart();
            map.forEach((k, v) -> {
                try {
                    stream.writeObjectField(k);
                    stream.writeVal(v);
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            });
            stream.writeObjectEnd();
            return Arrays.copyOfRange(stream.buffer().data(), 0, stream.buffer().tail());
        } catch (IOException e) {
            throw new JsonException(e);
        } finally {
            JsonStreamPool.returnJsonStream(stream);
        }
    }

    /**
     * Serialize a list of objects into a JSON array and return it as a byte array.
     *
     * @param objs the list of objects
     * @return the byte array
     */
    public static byte[] serialize(List<?> objs) {
        JsonStream stream = JsonStreamPool.borrowJsonStream();
        try {
            stream.reset(null);
            stream.writeArrayStart();
            for (Object obj : objs) {
                stream.writeVal(obj.getClass(), obj);
            }
            stream.writeArrayEnd();
            return Arrays.copyOfRange(stream.buffer().data(), 0, stream.buffer().tail());
        } catch (IOException e) {
            throw new JsonException(e);
        } finally {
            JsonStreamPool.returnJsonStream(stream);
        }
    }
}
