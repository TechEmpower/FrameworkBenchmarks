package cn.ibaijia.tfb.protocol;

import cn.ibaijia.isocket.protocol.Protocol;
import cn.ibaijia.isocket.session.Session;
import cn.ibaijia.tfb.http.HttpEntity;
import cn.ibaijia.tfb.http.HttpRequestEntity;
import cn.ibaijia.tfb.http.HttpResponseEntity;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.ByteBuffer;

public class SimpleHttpProtocol implements Protocol<ByteBuffer, HttpEntity> {

    private static final Logger logger = LoggerFactory.getLogger(SimpleHttpProtocol.class);

    private static final String CONTENT_LENGTH = "CONTENT-LENGTH";
    private static final String TRANSFER_ENCODING = "TRANSFER-ENCODING";
    private static final String CHUNKED = "CHUNKED";
    private static final byte CR13 = (byte) 13; // \CR \r
    private static final byte LF10 = (byte) 10; // \LF \n
    private static final byte SPACE0 = (byte) 32; // \SP
    private static final byte COLON = (byte) 58; // \:

    private static final String httpEntityKey = "httpEntity";

    /**
     * 解析HTTP请求
     *
     * @param byteBuffer
     * @param session
     * @return
     */
    @Override
    public HttpEntity decode(ByteBuffer byteBuffer, Session session) {
        HttpRequestEntity httpEntity = (HttpRequestEntity) session.getAttribute(httpEntityKey);
        if (httpEntity == null) {
            httpEntity = new HttpRequestEntity();
            session.setAttribute(httpEntityKey, httpEntity);
        }

        if (!httpEntity.headerComplete() && byteBuffer.hasRemaining()) { //解析header
            readHeader(byteBuffer, session, httpEntity);
        }

        if (httpEntity.headerComplete() && httpEntity.bodyBuffer != null && byteBuffer.hasRemaining()) {// 解析body
            readBody(byteBuffer, session, httpEntity);
        }

        if (httpEntity.complete()) {
            session.setAttribute(httpEntityKey, null);
            return httpEntity;
        }

        return null;
    }

    private void readHeader(ByteBuffer byteBuffer, Session session, HttpRequestEntity httpEntity) {
        try {
            ByteBuffer buf = byteBuffer.duplicate();
            int startPos = 0;
            int endPos = 0;
            while (byteBuffer.hasRemaining()) {
                byte b = byteBuffer.get();
                endPos++;
                if (b == CR13) {
                    httpEntity.crNum++;
                } else if (b == LF10) {
                    httpEntity.lfNum++;
                } else {
                    httpEntity.crNum = 0;
                    httpEntity.lfNum = 0;
                }
                if (httpEntity.isReadHeadLine()) {
                    if (b == SPACE0) {
                        int len = endPos - startPos - 1;
                        byte[] bytes = new byte[len];
                        buf.get(bytes, 0, len);
                        startPos = endPos;
                        buf.position(startPos);
                        if (httpEntity.method == null) {
                            httpEntity.method = new String(bytes);
                            continue;
                        }
                        if (httpEntity.url == null) {
                            httpEntity.url = new String(bytes);
                            continue;
                        }
                    }
                    if (httpEntity.crNum == 1 && httpEntity.lfNum == 1) {
                        int len = endPos - startPos - 2;
                        byte[] bytes = new byte[len];
                        buf.get(bytes, 0, len);
                        startPos = endPos;
                        buf.position(startPos);
                        httpEntity.protocol = new String(bytes);
                        continue;
                    }
                } else {
                    if (b == COLON && httpEntity.tmp == null) {
                        int len = endPos - startPos - 1;
                        byte[] bytes = new byte[len];
                        buf.get(bytes, 0, len);
                        startPos = endPos;
                        buf.position(startPos);
                        httpEntity.tmp = new String(bytes);
                        continue;
                    }
                    if (httpEntity.crNum == 1 && httpEntity.lfNum == 1) {
                        int len = endPos - startPos - 2;
                        byte[] bytes = new byte[len];
                        buf.get(bytes, 0, len);
                        startPos = endPos;
                        buf.position(startPos);
                        String value = new String(bytes);
                        httpEntity.setHeader(httpEntity.tmp, value);
                        httpEntity.tmp = null;
                        if (CONTENT_LENGTH.equals(httpEntity.tmp)) {
                            httpEntity.contentLength = (value == null ? 0 : Integer.valueOf(value));
                            httpEntity.bodyBuffer = ByteBuffer.allocate(httpEntity.contentLength);//TODO can pooling
                        }
                        if (CHUNKED.equals(httpEntity.tmp)) {
                            httpEntity.chunked = true;
//                            throw new RuntimeException("not support chunked");
                        }
                    }
                }
            }
        } catch (Exception e) {
            logger.error("readHeader error.", e);
        }
    }

    private void readBody(ByteBuffer byteBuffer, Session session, HttpRequestEntity httpEntity) {
        try {
            if (httpEntity.bodyBuffer.hasRemaining()) {
                if (byteBuffer.remaining() <= httpEntity.bodyBuffer.remaining()) {
                    httpEntity.bodyBuffer.put(byteBuffer);
                } else {
                    byte[] bytes = new byte[httpEntity.bodyBuffer.remaining()];
                    byteBuffer.get(bytes);
                    httpEntity.bodyBuffer.put(bytes);
                }
            }
            if (!httpEntity.bodyBuffer.hasRemaining()) {
                httpEntity.processBody();
            }
        } catch (Exception e) {
            logger.error("readHeader error.", e);
        }
    }

    @Override
    public ByteBuffer encode(HttpEntity httpEntity, Session session) {
        HttpResponseEntity httpResponseEntity = (HttpResponseEntity) httpEntity;

        return httpResponseEntity.toBuffer();
    }
}
