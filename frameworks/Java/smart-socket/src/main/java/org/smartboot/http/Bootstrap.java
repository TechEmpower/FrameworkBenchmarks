/*
 * Copyright (c) 2018, org.smartboot. All rights reserved.
 * project name: smart-socket
 * file name: HttpBootstrap.java
 * Date: 2018-01-28
 * Author: sandao
 */

package org.smartboot.http;

import org.smartboot.http.server.handle.HttpHandle;
import org.smartboot.http.server.v2.HttpMessageProcessor;
import org.smartboot.http.server.v2.decode.Http11Request;
import org.smartboot.http.server.v2.decode.HttpRequestProtocol;
import org.smartboot.http.utils.HttpHeaderConstant;
import org.smartboot.socket.MessageProcessor;
import org.smartboot.socket.transport.AioQuickServer;

import java.io.IOException;

public class Bootstrap {
    static byte[] body = "Hello, World!".getBytes();

    public static void main(String[] args) {
        HttpMessageProcessor processor = new HttpMessageProcessor(System.getProperty("webapps.dir", "./"));
        processor.route("/plaintext", new HttpHandle() {


            @Override
            public void doHandle(HttpRequest request, HttpResponse response) throws IOException {

                response.setHeader(HttpHeaderConstant.Names.CONTENT_LENGTH, body.length + "");
                response.setHeader(HttpHeaderConstant.Names.CONTENT_TYPE, "text/plain; charset=UTF-8");
                response.getOutputStream().write(body);
            }
        });
        processor.route("/json", new HttpHandle() {
            @Override
            public void doHandle(HttpRequest request, HttpResponse response) throws IOException {
                byte[] b = JSON.toJson(new Message("Hello, World!"));
                response.setHeader(HttpHeaderConstant.Names.CONTENT_LENGTH, b.length + "");
                response.setHeader(HttpHeaderConstant.Names.CONTENT_TYPE, "application/json");
                response.getOutputStream().write(b);
            }
        });
        http(processor);
//        https(processor);
    }

    public static void http(MessageProcessor<Http11Request> processor) {
        // 定义服务器接受的消息类型以及各类消息对应的处理器
        AioQuickServer<Http11Request> server = new AioQuickServer<>(8080, new HttpRequestProtocol(), processor);
        server.setWriteQueueSize(1024);
        server.setReadBufferSize(1024*4);
//        server.setThreadNum((int) (Runtime.getRuntime().availableProcessors() * 2));
        try {
            server.start();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
