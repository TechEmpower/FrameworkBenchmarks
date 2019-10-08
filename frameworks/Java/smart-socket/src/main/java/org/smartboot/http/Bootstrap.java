/*
 * Copyright (c) 2018, org.smartboot. All rights reserved.
 * project name: smart-socket
 * file name: HttpBootstrap.java
 * Date: 2018-01-28
 * Author: sandao
 */

package org.smartboot.http;

import com.jsoniter.output.JsonStream;
import com.jsoniter.output.JsonStreamPool;
import com.jsoniter.spi.JsonException;
import org.smartboot.http.server.HttpMessageProcessor;
import org.smartboot.http.server.decode.Http11Request;
import org.smartboot.http.server.decode.HttpRequestProtocol;
import org.smartboot.http.server.handle.HttpHandle;
import org.smartboot.socket.MessageProcessor;
import org.smartboot.socket.StateMachineEnum;
import org.smartboot.socket.extension.plugins.MonitorPlugin;
import org.smartboot.socket.extension.processor.AbstractMessageProcessor;
import org.smartboot.socket.transport.AioQuickServer;
import org.smartboot.socket.transport.AioSession;

import java.io.IOException;

public class Bootstrap {
    static byte[] body = "Hello, World!".getBytes();

    public static void main(String[] args) {
        System.setProperty("smart-socket.server.pageSize", (8 * 1024 * 1024) + "");
//        System.setProperty("smart-socket.bufferPool.pageNum", 16 + "");
        System.setProperty("smart-socket.session.writeChunkSize", (1024 * 4) + "");
//        System.setProperty("sun.nio.ch.maxCompletionHandlersOnStack","24");
        HttpMessageProcessor processor = new HttpMessageProcessor(System.getProperty("webapps.dir", "./"));
        processor.route("/plaintext", new HttpHandle() {


            @Override
            public void doHandle(HttpRequest request, HttpResponse response) throws IOException {
                response.setContentLength(body.length);
                response.setContentType("text/plain; charset=UTF-8");
                response.getOutputStream().write(body);
            }
        });
        processor.route("/json", new HttpHandle() {

            @Override
            public void doHandle(HttpRequest request, HttpResponse response) throws IOException {

                response.setContentType("application/json");
                JsonStream stream = JsonStreamPool.borrowJsonStream();
                try {
                    stream.reset(null);
                    stream.writeVal(Message.class, new Message("Hello, World!"));
                    response.setContentLength(stream.buffer().tail());
                    response.getOutputStream().write(stream.buffer().data(), 0, stream.buffer().tail());
                } catch (IOException e) {
                    throw new JsonException(e);
                } finally {
                    JsonStreamPool.returnJsonStream(stream);
                }
            }
        });
        http(processor);
//        https(processor);
    }

    public static void http(final MessageProcessor<Http11Request> processor) {
        AbstractMessageProcessor messageProcessor = new AbstractMessageProcessor<Http11Request>() {
            @Override
            public void process0(AioSession<Http11Request> session, Http11Request msg) {
                processor.process(session, msg);
            }

            @Override
            public void stateEvent0(AioSession<Http11Request> session, StateMachineEnum stateMachineEnum, Throwable throwable) {
                processor.stateEvent(session, stateMachineEnum, throwable);
            }
        };
        messageProcessor.addPlugin(new MonitorPlugin(5  ));
        // 定义服务器接受的消息类型以及各类消息对应的处理器
        AioQuickServer<Http11Request> server = new AioQuickServer<>(8080, new HttpRequestProtocol(), messageProcessor);
        server.setReadBufferSize(1024 * 4);
        int cpuNum = Runtime.getRuntime().availableProcessors();
        server.setThreadNum(cpuNum + 2);
        try {
            server.start();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
