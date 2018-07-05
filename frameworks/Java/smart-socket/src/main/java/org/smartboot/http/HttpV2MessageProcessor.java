package org.smartboot.http;

import org.smartboot.socket.MessageProcessor;
import org.smartboot.socket.StateMachineEnum;
import org.smartboot.socket.transport.AioSession;

import java.io.IOException;
import java.nio.ByteBuffer;

/**
 * @author 三刀
 * @version V1.0 , 2018/6/10
 */
public class HttpV2MessageProcessor implements MessageProcessor<HttpEntityV2> {
    private static String b = "HTTP/1.1 200 OK\r\n" +
            "Server:smart-socket\r\n" +
            "Connection:keep-alive\r\n" +
            "Host:localhost\r\n" +
            "Content-Length:31\r\n" +
            "Date:Wed, 11 Apr 2018 12:35:01 GMT\r\n\r\n" +
            "Hello smart-socket http server!";

    private byte[] plainText;
    private byte[] json;

    public HttpV2MessageProcessor() {
        plainText = ("HTTP/1.1 200 OK\r\n" +
                "Content-Length: 13\r\n" +
                "Content-Type: text/plain; charset=UTF-8\r\n" +
                "Server: smart-socket\r\n" +
                "Date: Wed, 17 Apr 2013 12:00:00 GMT\r\n" +
                "\r\n" +
                "Hello, World!").getBytes();

        json = ("HTTP/1.1 200 OK\r\n" +
                "Content-Type: application/json\r\n" +
                "Content-Length: 27\r\n" +
                "Server: Example\r\n" +
                "Date: Wed, 17 Apr 2013 12:00:00 GMT\r\n" +
                "\r\n" +
                "{\"message\":\"Hello, World!\"}").getBytes();
    }

    @Override
    public void process(AioSession<HttpEntityV2> session, HttpEntityV2 msg) {
        try {
            String uri = new String(msg.getBytes(msg.uri));
            if ("/json".equals(uri)) {
                session.write(ByteBuffer.wrap(json));
            } else if ("/plaintext".equals(uri)) {
                session.write(ByteBuffer.wrap(plainText));
            } else {
                session.write(ByteBuffer.wrap(b.getBytes()));
            }
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            msg.rest();
        }
    }

    @Override
    public void stateEvent(AioSession<HttpEntityV2> session, StateMachineEnum stateMachineEnum, Throwable throwable) {
        switch (stateMachineEnum) {
            case NEW_SESSION:
                session.setAttachment(new HttpEntityV2());
                break;
            case PROCESS_EXCEPTION:
                session.close();
                break;
        }
    }
}
