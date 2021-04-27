package cn.ibaijia.tfb.http;

import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.Map;

public class HttpRequestEntity extends HttpEntity {

    public ByteBuffer headerBuffer = ByteBuffer.allocate(4 * 1024);
    public ByteBuffer bodyBuffer = null;
    public boolean chunked = false;
    public int contentLength = -1;
    public int crlfNum = 0;

    private static final String CONTENT_LENGTH = "CONTENT-LENGTH";
    private static final String TRANSFER_ENCODING = "TRANSFER-ENCODING";
    private static final String CHUNKED = "CHUNKED";

    //请求行
    private String headLine;
    public String method;
    public String url;
    public String protocol;

    //请求体
    public String body;
    //第一次 请求header时解析 第一行不要
    private String[] headersArr;
    private Map<String, String> headers;

    public boolean headerComplete() {
        return crlfNum == 4;
    }

    public void processHeader() {
        headerBuffer.flip();
        byte[] bytes = new byte[headerBuffer.remaining()];
        headerBuffer.get(bytes);
        String str = new String(bytes);
        headersArr = str.split("\r\n");
        if (headersArr.length > 0) {
            headLine = headersArr[0];
            String[] arr = headLine.split(" ");
            method = arr[0];
            url = arr[1];
            protocol = arr[2];
            //
            String lengthStr = getHeader(CONTENT_LENGTH);
            contentLength = lengthStr == null ? 0 : Integer.valueOf(lengthStr);
            chunked = CHUNKED.equalsIgnoreCase(getHeader(TRANSFER_ENCODING));
            if (chunked) {
                //TODO
                throw new RuntimeException("not support chunked");
            }
            if (contentLength > 0) {
                bodyBuffer = ByteBuffer.allocate(contentLength);
            }
        }
    }

    private Map<String, String> getHeaders() {
        if (headers == null) {
            headers = new HashMap<>();
            boolean firstLine = true;
            for (String header : headersArr) {
                if (firstLine) {
                    continue;
                }
                String[] arr = header.split(":", 2);
                headers.put(arr[0], arr[1]);
            }
        }
        return headers;
    }

    public String getHeader(String name) {
        return getHeaders().get(name);
    }

    @Override
    public void setHeader(String name, String value) {
        // TODO
    }

    public void processBody() {
        bodyBuffer.flip();
        byte[] bytes = new byte[bodyBuffer.remaining()];
        bodyBuffer.get(bytes);
        body = new String(bytes);
    }

    public boolean complete() {
        if (contentLength == 0) {
            return true;
        } else {
            return body != null;
        }
    }
}
