package cn.ibaijia.tfb.http;

import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.Map;

public class HttpRequestEntity extends HttpEntity {



    public ByteBuffer bodyBuffer = null;
    public boolean chunked = false;
    public int contentLength = -1;
    public int crNum = 0;
    public int lfNum = 0;
    public String tmp;


    //请求行
    public String method;
    public String url;
    public String protocol;

    //请求体
    public String body;
    //第一次 请求header时解析 第一行不要
    private Map<String, String> headers = new HashMap<>();

    public String getHeader(String name) {
        return this.headers.get(name);
    }

    @Override
    public void setHeader(String name, String value) {
        this.headers.put(name, value);
    }

    public void processBody() {
        bodyBuffer.flip();
        byte[] bytes = new byte[bodyBuffer.remaining()];
        bodyBuffer.get(bytes);
        body = new String(bytes);
    }

    public boolean complete() {
        if (contentLength < 1) {
            return true;
        } else {
            return body != null;
        }
    }

    public boolean headerComplete() {
        return this.crNum == 2 && this.lfNum == 2;
    }

    public boolean isReadHeadLine() {
        return this.protocol == null;
    }
}
