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
import org.smartboot.http.server.Http11Request;
import org.smartboot.http.server.HttpMessageProcessor;
import org.smartboot.http.server.HttpRequestProtocol;
import org.smartboot.http.server.handle.HttpHandle;
import org.smartboot.http.server.handle.RouteHandle;
import org.smartboot.socket.StateMachineEnum;
import org.smartboot.socket.extension.plugins.MonitorPlugin;
import org.smartboot.socket.extension.plugins.SocketOptionPlugin;
import org.smartboot.socket.extension.processor.AbstractMessageProcessor;
import org.smartboot.socket.transport.AioQuickServer;
import org.smartboot.socket.transport.AioSession;

import java.io.IOException;

public class Bootstrap {
    static byte[] body = "Hello, World!".getBytes();

    public static void main(String[] args) {
//        System.setProperty("sun.nio.ch.maxCompletionHandlersOnStack", "32");
        RouteHandle routeHandle = new RouteHandle();
        routeHandle.route("/plaintext", new HttpHandle() {


            @Override
            public void doHandle(HttpRequest request, HttpResponse response) throws IOException {
                response.setContentLength(body.length);
                response.setContentType("text/plain; charset=UTF-8");
                response.write(body);
            }
        }).route("/json", new HttpHandle() {

            @Override
            public void doHandle(HttpRequest request, HttpResponse response) throws IOException {

                response.setContentType("application/json");
                JsonStream stream = JsonStreamPool.borrowJsonStream();
                try {
                    stream.reset(null);
                    stream.writeVal(Message.class, new Message("Hello, World!"));
                    response.setContentLength(stream.buffer().tail());
                    response.getOutputStream().write(stream.buffer().data(), 0, stream.buffer().tail());
                    response.getOutputStream().flush();
                } catch (IOException e) {
                    throw new JsonException(e);
                } finally {
                    JsonStreamPool.returnJsonStream(stream);
                }
            }
        });
        HttpMessageProcessor processor = new HttpMessageProcessor();
        processor.pipeline(routeHandle);
        http(processor);
//        https(processor);
    }

    public static void http(final HttpMessageProcessor processor) {
        AbstractMessageProcessor<Http11Request> messageProcessor = new AbstractMessageProcessor<Http11Request>() {
            @Override
            public void process0(AioSession session, Http11Request msg) {
                processor.process(session, msg);
            }

            @Override
            public void stateEvent0(AioSession session, StateMachineEnum stateMachineEnum, Throwable throwable) {
                processor.stateEvent(session, stateMachineEnum, throwable);
            }
        };
        messageProcessor.addPlugin(new MonitorPlugin(5));
//        messageProcessor.addPlugin(new SocketOptionPlugin());

        int cpuNum = Runtime.getRuntime().availableProcessors();
        // 定义服务器接受的消息类型以及各类消息对应的处理器
        AioQuickServer<Http11Request> server = new AioQuickServer<>(8080, new HttpRequestProtocol(), messageProcessor);
        server.setThreadNum(cpuNum + 2)
                .setReadBufferSize(1024 * 4)
                .setBufferPoolPageSize(10 * 1024 * 1024)
                .setBufferPoolSharedPageSize(64 * 1024 * 1024)
                .setBufferPoolChunkSize(1024 * 4);

//        messageProcessor.addPlugin(new BufferPageMonitorPlugin(server, 6));
        try {
            server.start();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
