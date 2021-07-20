package cn.ibaijia.tfb.http;

import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.Map;

public class HttpResponseEntity extends HttpEntity {
    public static final String protocol = "HTTP/1.1";
    public int statusCode = 200;
    public String status = "OK";

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

    public int getStatusCode() {
        return statusCode;
    }

    public void setStatusCode(int statusCode) {
        this.statusCode = statusCode;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public ByteBuffer toBuffer() {
        StringBuilder sb = new StringBuilder();
        sb.append(protocol).append(" ").append(statusCode).append(" ").append(status).append("\r\n");
        sb.append("Content-Length:").append(body.length()).append("\r\n");
        for (Map.Entry<String, String> header : headers.entrySet()) {
            sb.append(header.getKey()).append(":").append(header.getValue()).append("\r\n");
        }
        sb.append("\r\n").append(body);
        return ByteBuffer.wrap(sb.toString().getBytes());
    }
}
