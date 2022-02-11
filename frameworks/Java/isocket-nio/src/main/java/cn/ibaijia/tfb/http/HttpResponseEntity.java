package cn.ibaijia.tfb.http;

import cn.ibaijia.tfb.Consts;
import cn.ibaijia.tfb.DateUtil;

import java.nio.ByteBuffer;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * @author longzl
 */
public class HttpResponseEntity extends HttpEntity {
    private static final byte[] PROTOCOL = "HTTP/1.1 ".getBytes();
    private static final byte[] STATUS_200 = "200 OK".getBytes();
    private static final byte[] CRLF = "\r\n".getBytes();
    private static final byte[] COLON = ":".getBytes();
    private static final byte[] SERVER_NAME = "\r\nServer:tfb\r\n".getBytes();
    private static final byte[] CONTENT_LENGTH_HEAD = "\r\nContent-Length:".getBytes();
    private static final byte[] CONTENT_TYPE_HEAD = "\r\nContent-Type:".getBytes();

    private byte[] contentType = Consts.TEXT_TYPE;
    /**
     * 响应体
     */
    public String body;

    /**
     * 请求头 或者 响应头
     */
    public Map<byte[], byte[]> headers = new HashMap<>();

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
        headers.put(name, value);
    }

    @Override
    public void setContentType(String contentType) {
        this.contentType = contentType.getBytes();
    }

    @Override
    public void setContentType(byte[] contentType) {
        this.contentType = contentType;
    }

    public ByteBuffer toBuffer(ByteBuffer byteBuffer) {
        byteBuffer.put(PROTOCOL);
        byteBuffer.put(STATUS_200);
        byteBuffer.put(DateUtil.getDate());
        byteBuffer.put(CONTENT_LENGTH_HEAD);
        Integer length = body.length();
        byteBuffer.put(length.toString().getBytes());
        byteBuffer.put(CONTENT_TYPE_HEAD);
        byteBuffer.put(contentType);
        byteBuffer.put(SERVER_NAME);
        for (Map.Entry<byte[], byte[]> header : headers.entrySet()) {
            byteBuffer.put(header.getKey());
            byteBuffer.put(COLON);
            byteBuffer.put(header.getValue());
            byteBuffer.put(CRLF);
        }
        byteBuffer.put(CRLF);
        byteBuffer.put(body.getBytes());
        byteBuffer.flip();
        return byteBuffer;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        HttpResponseEntity that = (HttpResponseEntity) o;
        return Objects.equals(body, that.body) &&
                Objects.equals(headers, that.headers);
    }

    @Override
    public int hashCode() {

        return Objects.hash(body, headers);
    }
}
