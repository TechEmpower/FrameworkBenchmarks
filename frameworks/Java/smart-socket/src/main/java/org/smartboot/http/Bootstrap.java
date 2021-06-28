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
import org.smartboot.Message;
import org.smartboot.http.server.HttpBootstrap;
import org.smartboot.http.server.HttpRequest;
import org.smartboot.http.server.HttpResponse;
import org.smartboot.http.server.HttpServerHandler;
import org.smartboot.http.server.handler.HttpRouteHandler;
import org.smartboot.http.server.impl.Request;
import org.smartboot.socket.StateMachineEnum;
import org.smartboot.socket.extension.processor.AbstractMessageProcessor;
import org.smartboot.socket.transport.AioSession;

import javax.sql.DataSource;
import java.io.IOException;

public class Bootstrap {
    static byte[] body = "Hello, World!".getBytes();

    public static void main(String[] args) {
        HttpRouteHandler routeHandle = new HttpRouteHandler();
        routeHandle
                .route("/plaintext", new HttpServerHandler() {


                    @Override
                    public void handle(HttpRequest request, HttpResponse response) throws IOException {
                        response.setContentLength(body.length);
                        response.setContentType("text/plain; charset=UTF-8");
                        response.write(body);
                    }
                })
                .route("/json", new HttpServerHandler() {

                    @Override
                    public void handle(HttpRequest request, HttpResponse response) throws IOException {

                        response.setContentType("application/json");
                        JsonUtil.writeJsonBytes(response, new Message("Hello, World!"));
                    }
                });
        initDB(routeHandle);
        int cpuNum = Runtime.getRuntime().availableProcessors();
        // 定义服务器接受的消息类型以及各类消息对应的处理器
        HttpBootstrap bootstrap = new HttpBootstrap();
        bootstrap.configuration()
                .threadNum(cpuNum)
                .readBufferSize(1024 * 4)
                .writeBufferSize(1024 * 4)
                .readMemoryPool(16384 * 1024 * 4)
                .writeMemoryPool(10 * 1024 * 1024 * cpuNum, cpuNum)
                .messageProcessor(processor -> new AbstractMessageProcessor<>() {
                    @Override
                    public void process0(AioSession session, Request msg) {
                        processor.process(session, msg);
                    }

                    @Override
                    public void stateEvent0(AioSession session, StateMachineEnum stateMachineEnum, Throwable throwable) {
                        processor.stateEvent(session, stateMachineEnum, throwable);
                    }
                });
        bootstrap.pipeline(routeHandle).setPort(8080).start();
    }

    private static void initDB(HttpRouteHandler routeHandle) {
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
        config.addDataSourceProperty("cachePrepStmts", "true");
        config.addDataSourceProperty("prepStmtCacheSize", "250");
        config.addDataSourceProperty("prepStmtCacheSqlLimit", "2048");
        DataSource dataSource = new HikariDataSource(config);
        routeHandle.route("/db", new SingleQueryHandler(dataSource))
                .route("/queries", new MultipleQueriesHandler(dataSource))
                .route("/updates", new UpdateHandler(dataSource));
//                .route("/fortunes", new FortunesHandler(dataSource));
    }
}
