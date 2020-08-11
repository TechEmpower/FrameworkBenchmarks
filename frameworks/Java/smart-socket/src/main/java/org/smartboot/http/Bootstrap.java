/*
 * Copyright (c) 2018, org.smartboot. All rights reserved.
 * project name: smart-socket
 * file name: HttpBootstrap.java
 * Date: 2018-01-28
 * Author: sandao
 */

package org.smartboot.http;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import org.smartboot.aio.EnhanceAsynchronousChannelProvider;
import org.smartboot.http.server.HttpMessageProcessor;
import org.smartboot.http.server.HttpRequestProtocol;
import org.smartboot.http.server.Request;
import org.smartboot.http.server.handle.HttpHandle;
import org.smartboot.http.server.handle.HttpRouteHandle;
import org.smartboot.socket.StateMachineEnum;
import org.smartboot.socket.buffer.BufferFactory;
import org.smartboot.socket.buffer.BufferPagePool;
import org.smartboot.socket.extension.processor.AbstractMessageProcessor;
import org.smartboot.socket.transport.AioQuickServer;
import org.smartboot.socket.transport.AioSession;

import javax.sql.DataSource;
import java.io.IOException;

public class Bootstrap {
    static byte[] body = "Hello, World!".getBytes();

    public static void main(String[] args) {
        System.setProperty("java.nio.channels.spi.AsynchronousChannelProvider", EnhanceAsynchronousChannelProvider.class.getName());

        HttpRouteHandle routeHandle = new HttpRouteHandle();
        routeHandle
                .route("/plaintext", new HttpHandle() {


                    @Override
                    public void doHandle(HttpRequest request, HttpResponse response) throws IOException {
                        response.setContentLength(body.length);
                        response.setContentType("text/plain; charset=UTF-8");
                        response.write(body);
                    }
                })
                .route("/json", new HttpHandle() {

                    @Override
                    public void doHandle(HttpRequest request, HttpResponse response) throws IOException {

                        response.setContentType("application/json");
                        JsonUtil.writeJsonBytes(response, new Message("Hello, World!"));
                    }
                });
        initDB(routeHandle);
        HttpMessageProcessor processor = new HttpMessageProcessor();
        processor.pipeline(routeHandle);
        http(processor);
    }

    public static void http(final HttpMessageProcessor processor) {
        AbstractMessageProcessor<Request> messageProcessor = new AbstractMessageProcessor<Request>() {
            @Override
            public void process0(AioSession session, Request msg) {
                processor.process(session, msg);
            }

            @Override
            public void stateEvent0(AioSession session, StateMachineEnum stateMachineEnum, Throwable throwable) {
                processor.stateEvent(session, stateMachineEnum, throwable);
            }
        };
//        messageProcessor.addPlugin(new MonitorPlugin(5));
//        messageProcessor.addPlugin(new SocketOptionPlugin());

        int cpuNum = Runtime.getRuntime().availableProcessors();
        // 定义服务器接受的消息类型以及各类消息对应的处理器
        AioQuickServer<Request> server = new AioQuickServer<>(8080, new HttpRequestProtocol(), messageProcessor);
        server.setThreadNum(cpuNum + 2)
                .setReadBufferSize(1024 * 4)
                .setBufferFactory(new BufferFactory() {
                    @Override
                    public BufferPagePool create() {
                        return new BufferPagePool(10 * 1024 * 1024, cpuNum + 2, 64 * 1024 * 1024, true);
                    }
                })
                .setWriteBuffer(1024 * 4, 8);

//        messageProcessor.addPlugin(new BufferPageMonitorPlugin(server, 6));
        try {
            server.start();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static void initDB(HttpRouteHandle routeHandle) {
        try {
            Class.forName("org.postgresql.Driver");
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
        HikariConfig config = new HikariConfig();
        config.setJdbcUrl("jdbc:postgresql://tfb-database:5432/hello_world");
        config.setUsername("benchmarkdbuser");
        config.setPassword("benchmarkdbpass");
        config.setMaximumPoolSize(64);
        DataSource dataSource = new HikariDataSource(config);
        routeHandle.route("/db", new SingleQueryHandler(dataSource))
                .route("/queries", new MultipleQueriesHandler(dataSource))
                .route("/updates", new UpdateHandler(dataSource));
    }
}
