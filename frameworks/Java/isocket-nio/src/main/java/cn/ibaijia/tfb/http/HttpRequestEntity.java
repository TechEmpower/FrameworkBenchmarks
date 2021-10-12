package cn.ibaijia.tfb.http;

import cn.ibaijia.tfb.Consts;

import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

public class HttpRequestEntity extends HttpEntity {


    public ByteBuffer bodyBuffer = null;
    public boolean chunked = false;
    public int contentLength = -1;
    public int crNum = 0;
    public int lfNum = 0;
    public byte[] tmp;


    //请求行
    public String method;
    public String url;
    public String protocol;

    //请求体
    public String body;
    //第一次 请求header时解析 第一行不要
    private Map<byte[], byte[]> headers = new HashMap<>();
    private byte[] contentType = Consts.TEXT_TYPE;

    @Override
    public byte[] getHeader(byte[] name) {
        for (Map.Entry<byte[], byte[]> entry : headers.entrySet()) {
            if (Arrays.equals(entry.getKey(), name)) {
                return entry.getValue();
            }
        }
        return null;
    }

    @Override
    public byte[] getHeader(String name) {
        return getHeader(name.getBytes());
    }

    @Override
    public void setHeader(byte[] name, byte[] value) {
        this.headers.put(name, value);
    }

    @Override
    public void setContentType(String contentType) {
        this.contentType = contentType.getBytes();
    }

    @Override
    public void setContentType(byte[] contentType) {
        this.contentType = contentType;
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
