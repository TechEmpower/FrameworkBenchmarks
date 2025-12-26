package hello;

import com.alibaba.fastjson2.JSON;

public final class JsonUtils {

    private JsonUtils() {
    }

    public static byte[] serializeMsg(Message obj) {
        return JSON.toJSONBytes(obj);
    }
}
