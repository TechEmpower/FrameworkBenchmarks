package cn.ibaijia.tfb.http;

import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class HttpResponseEntity extends HttpEntity {
    public static final String protocol = "HTTP/1.1";
    public static final byte[] CONTENT_LENGTH = "Content-Length:".getBytes();
    public static final byte[] CRLF = "\r\n".getBytes();
    public static final byte[] BLANK = " ".getBytes();
    public static final byte[] COLON = ":".getBytes();
    public static final byte[] STATUS_200 = "200".getBytes();
    public static final byte[] STATUS_OK = "OK".getBytes();

    //响应体
    public String body = "";

    //请求头 或者 响应头
    public Map<String, String> headers = new HashMap<>();

    @Override
    public String getHeader(String name) {
        return headers.get(name);
    }

    @Override
    public void setHeader(String name, String value) {
        headers.put(name, value);
    }

//    public int getStatusCode() {
//        return statusCode;
//    }
//
//    public void setStatusCode(int statusCode) {
//        this.statusCode = statusCode;
//    }
//
//    public String getStatus() {
//        return status;
//    }
//
//    public void setStatus(String status) {
//        this.status = status;
//    }

//    public ByteBuffer toBuffer() {
//        StringBuilder sb = new StringBuilder();
//        sb.append(protocol).append(" ").append(STATUS_200).append(" ").append(STATUS_OK).append("\r\n");
//        sb.append("Content-Length:").append(body.length()).append("\r\n");
//        for (Map.Entry<String, String> header : headers.entrySet()) {
//            sb.append(header.getKey()).append(":").append(header.getValue()).append("\r\n");
//        }
//        sb.append("\r\n").append(body);
//        return ByteBuffer.wrap(sb.toString().getBytes());
//    }

    public ByteBuffer toBuffer(ByteBuffer byteBuffer) {
        byteBuffer.put(protocol.getBytes());
        byteBuffer.put(BLANK);
        byteBuffer.put(STATUS_200);
        byteBuffer.put(BLANK);
        byteBuffer.put(STATUS_OK);
        byteBuffer.put(CRLF);
        byteBuffer.put(CONTENT_LENGTH);
        byteBuffer.put(String.valueOf(body.length()).getBytes());
        byteBuffer.put(CRLF);
        for (Map.Entry<String, String> header : headers.entrySet()) {
            byteBuffer.put(header.getKey().getBytes());
            byteBuffer.put(COLON);
            byteBuffer.put(header.getValue().getBytes());
            byteBuffer.put(CRLF);
        }
        byteBuffer.put(CRLF);
        byteBuffer.put(body.getBytes());
        byteBuffer.flip();
        return byteBuffer;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        HttpResponseEntity that = (HttpResponseEntity) o;
        return Objects.equals(body, that.body) &&
                Objects.equals(headers, that.headers);
    }

    @Override
    public int hashCode() {

        return Objects.hash(body, headers);
    }
}
