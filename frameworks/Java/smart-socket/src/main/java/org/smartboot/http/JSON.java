package org.smartboot.http;

import com.alibaba.fastjson.JSONObject;

/**
 * @author 三刀
 * @version V1.0 , 2018/8/12
 */
public class JSON {
    public static byte[] toJson(Object obj) {
        return JSONObject.toJSONBytes(obj);
    }
}
