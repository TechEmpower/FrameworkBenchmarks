package org.smartboot.http;

import com.alibaba.fastjson.JSONObject;

import java.util.HashMap;
import java.util.Map;

/**
 * @author 三刀
 * @version V1.0 , 2018/8/12
 */
public class JSON {
    private static Map<String, byte[]> cacheMap = new HashMap<>();

    public static byte[] toJson(Message obj) {
        byte[] cache = cacheMap.get(obj.getMessage());
        if (cache == null) {
            cache = JSONObject.toJSONBytes(obj);
            cacheMap.put(obj.getMessage(), cache);
        }
        return cache;
    }
}
