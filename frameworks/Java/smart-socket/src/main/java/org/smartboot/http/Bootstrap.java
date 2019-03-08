/*
 * Copyright (c) 2018, org.smartboot. All rights reserved.
 * project name: smart-socket
 * file name: HttpBootstrap.java
 * Date: 2018-01-28
 * Author: sandao
 */

package org.smartboot.http;

import org.smartboot.http.server.HttpMessageProcessor;
import org.smartboot.http.server.decode.Http11Request;
import org.smartboot.http.server.decode.HttpRequestProtocol;
import org.smartboot.http.server.handle.HttpHandle;
import org.smartboot.socket.MessageProcessor;
import org.smartboot.socket.transport.AioQuickServer;

import java.io.IOException;

public class Bootstrap {
    static byte[] body = "Hello, World!".getBytes();

    public static void main(String[] args) {
        System.setProperty("smart-socket.server.pageSize", (16 * 1024 * 1024) + "");
        System.setProperty("smart-socket.session.writeChunkSize", (1024 * 8) + "");
        System.setProperty("sun.nio.ch.maxCompletionHandlersOnStack","8");
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
                byte[] b = JSON.toJson(new Message("Hello, World!"));
                response.setContentLength(b.length);
                response.setContentType("application/json");
                response.getOutputStream().write(b);
            }
        });
        http(processor);
//        https(processor);
    }

    public static void http(MessageProcessor<Http11Request> processor) {
        // 定义服务器接受的消息类型以及各类消息对应的处理器
        AioQuickServer<Http11Request> server = new AioQuickServer<>(8080, new HttpRequestProtocol(), processor);
        server.setReadBufferSize(1024 * 4);
//        server.setThreadNum((int)(Runtime.getRuntime().availableProcessors() * 1.5));
        try {
            server.start();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
