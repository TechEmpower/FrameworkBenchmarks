package io.helidon.benchmark.nima;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import com.jsoniter.output.JsonStream;
import com.jsoniter.output.JsonStreamPool;
import com.jsoniter.spi.JsonException;

public class JsonSerializer {

    private JsonSerializer() {
    }

    /**
     * Serialize an instance into a byte array.
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
        }
    }

    /**
     * Serialize an instance into a JSON stream.
     *
     * @param obj the instance
     * @param stream the JSON stream
     */
    public static void serialize(Object obj, JsonStream stream) {
        try {
            stream.reset(null);
            stream.writeVal(obj.getClass(), obj);
        } catch (IOException e) {
            throw new JsonException(e);
        }
    }

    /**
     * Serialize a map of strings into a JSON stream.
     *
     * @param map the map
     * @param stream the JSON stream
     */
    public static void serialize(Map<String, String> map, JsonStream stream) {
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
        } catch (IOException e) {
            throw new JsonException(e);
        }
    }

    /**
     * Serialize a list of objects into a JSON stream.
     *
     * @param objs the list of objects
     * @param stream the JSON stream
     */
    public static void serialize(List<?> objs, JsonStream stream) {
        try {
            stream.reset(null);
            stream.writeArrayStart();
            int i = 0;
            int n = objs.size();
            for (Object obj : objs) {
                stream.writeVal(obj.getClass(), obj);
                if (i++ < n - 1) {
                    stream.writeMore();
                }

            }
            stream.writeArrayEnd();
        } catch (IOException e) {
            throw new JsonException(e);
        }
    }
}
