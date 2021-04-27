package cn.ibaijia.tfb.protocol;

import cn.ibaijia.isocket.Server;
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

    private static final byte CRLF13 = (byte) 13; // \r
    private static final byte CRLF10 = (byte) 10; // \n

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

        if (byteBuffer.hasRemaining() && !httpEntity.headerComplete()) { //解析header
            readHeader(byteBuffer, session, httpEntity);
        }

        if (httpEntity.bodyBuffer != null && byteBuffer.hasRemaining() && httpEntity.headerComplete()) {// 解析body
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
            while (byteBuffer.hasRemaining()) {
                byte b = byteBuffer.get();
                if (b == CRLF10 || b == CRLF13) {
                    httpEntity.crlfNum++;
                } else {
                    httpEntity.crlfNum = 0;
                }
                httpEntity.headerBuffer.put(b);
                if (httpEntity.crlfNum == 4) {
                    //处理header
                    httpEntity.processHeader();
                    break;
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
