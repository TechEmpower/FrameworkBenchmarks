package cn.ibaijia.tfb.protocol;

import cn.ibaijia.isocket.protocol.Protocol;
import cn.ibaijia.isocket.session.Session;
import cn.ibaijia.tfb.http.HttpEntity;
import cn.ibaijia.tfb.http.HttpRequestEntity;
import cn.ibaijia.tfb.http.HttpResponseEntity;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.ByteBuffer;
import java.util.Arrays;

public class SimpleHttpProtocol implements Protocol<ByteBuffer, HttpEntity> {

    private static final Logger logger = LoggerFactory.getLogger(SimpleHttpProtocol.class);

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
            readHeader(byteBuffer, httpEntity);
        }

        if (httpEntity.headerComplete()) {
            if (httpEntity.complete()) {
                session.setAttribute(httpEntityKey, null);
                return httpEntity;
            }
            if (httpEntity.bodyBuffer != null && byteBuffer.hasRemaining()) { // 解析request body
                readBody(byteBuffer, httpEntity);
            }
        }
        return null;
    }

    private void readHeader(ByteBuffer byteBuffer, HttpRequestEntity httpEntity) {
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

                if (httpEntity.headerComplete()) {
                    return;
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
                        } else if (httpEntity.url == null) {
                            httpEntity.url = new String(bytes);
                        }
                    } else if (httpEntity.crNum == 1 && httpEntity.lfNum == 1) {
                        int len = endPos - startPos - 2;
                        byte[] bytes = new byte[len];
                        buf.get(bytes, 0, len);
                        startPos = endPos;
                        buf.position(startPos);
                        httpEntity.protocol = new String(bytes);
                    }
                } else {
                    if (b == COLON && httpEntity.tmp == null) {
                        int len = endPos - startPos - 1;
                        byte[] bytes = new byte[len];
                        buf.get(bytes, 0, len);
                        startPos = endPos;
                        buf.position(startPos);
                        httpEntity.tmp = bytes;
                    } else if (httpEntity.crNum == 1 && httpEntity.lfNum == 1) {
                        int len = endPos - startPos - 2;
                        byte[] bytes = new byte[len];
                        buf.get(bytes, 0, len);
                        startPos = endPos;
                        buf.position(startPos);
                        httpEntity.setHeader(httpEntity.tmp, bytes);
                        httpEntity.tmp = null;
//                        if (Arrays.equals(CONTENT_LENGTH, httpEntity.tmp)) {
//                            httpEntity.contentLength = (value == null ? 0 : Integer.valueOf(value));
//                            httpEntity.bodyBuffer = ByteBuffer.allocate(httpEntity.contentLength);//TODO can pooling
//                        }
//                        if (Arrays.equals(CHUNKED, httpEntity.tmp)) {
//                            httpEntity.chunked = true;
//                            throw new RuntimeException("not support chunked");
//                        }
                    }
                }
            }
        } catch (Exception e) {
            logger.error("readHeader error.", e);
        }
    }

    private void readBody(ByteBuffer byteBuffer, HttpRequestEntity httpEntity) {
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
        ByteBuffer byteBuffer = session.getHandler().getPooledByteBuff().get();
        HttpResponseEntity httpResponseEntity = (HttpResponseEntity) httpEntity;
        return httpResponseEntity.toBuffer(byteBuffer);
    }
}
