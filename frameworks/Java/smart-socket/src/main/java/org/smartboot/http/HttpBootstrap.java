/*
 * Copyright (c) 2018, org.smartboot. All rights reserved.
 * project name: smart-socket
 * file name: HttpBootstrap.java
 * Date: 2018-01-28
 * Author: sandao
 */

package org.smartboot.http;

import org.apache.commons.lang.math.NumberUtils;
import org.smartboot.http.common.HttpEntityV2;
import org.smartboot.http.common.HttpRequestProtocol;
import org.smartboot.http.common.utils.HttpHeaderConstant;
import org.smartboot.http.server.handle.HttpHandle;
import org.smartboot.http.server.http11.HttpResponse;
import org.smartboot.socket.MessageProcessor;
import org.smartboot.socket.transport.AioQuickServer;

import java.io.IOException;
import java.net.UnknownHostException;

public class HttpBootstrap {
    static byte[] body = "Hello, World!".getBytes();

    public static void main(String[] args) throws UnknownHostException {
        org.smartboot.http.server.HttpV2MessageProcessor processor = new org.smartboot.http.server.HttpV2MessageProcessor(System.getProperty("webapps.dir", "./"));
        processor.route("/plaintext", new HttpHandle() {


            @Override
            public void doHandle(HttpEntityV2 request, HttpResponse response) throws IOException {

                response.setHeader(HttpHeaderConstant.Names.CONTENT_LENGTH, body.length + "");
                response.setHeader(HttpHeaderConstant.Names.CONTENT_TYPE, "text/plain; charset=UTF-8");
                response.getOutputStream().write(body);
            }
        });
        processor.route("/json", new HttpHandle() {
            @Override
            public void doHandle(HttpEntityV2 request, HttpResponse response) throws IOException {
                byte[] b = JSON.toJson(new Message("Hello, World!"));
                response.setHeader(HttpHeaderConstant.Names.CONTENT_LENGTH, b.length + "");
                response.setHeader(HttpHeaderConstant.Names.CONTENT_TYPE, "application/json");
                response.getOutputStream().write(b);
            }
        });
        http(processor);
//        https(processor);
    }

    public static void http(MessageProcessor<org.smartboot.http.common.HttpEntityV2> processor) {
        // 定义服务器接受的消息类型以及各类消息对应的处理器
        int port = NumberUtils.toInt(System.getProperty("port"), 8888);
        AioQuickServer<org.smartboot.http.common.HttpEntityV2> server = new AioQuickServer<org.smartboot.http.common.HttpEntityV2>(port, new HttpRequestProtocol(), processor);
        server.setWriteQueueSize(4);
        server.setReadBufferSize(1024);
//        server.setThreadNum(8);
        try {
            server.start();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
