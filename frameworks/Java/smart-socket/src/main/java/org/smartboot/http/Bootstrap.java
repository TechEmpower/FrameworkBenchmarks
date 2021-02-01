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
import org.smartboot.aio.EnhanceAsynchronousChannelProvider;
import org.smartboot.http.server.Request;
import org.smartboot.http.server.handle.HttpHandle;
import org.smartboot.http.server.handle.HttpRouteHandle;
import org.smartboot.socket.StateMachineEnum;
import org.smartboot.socket.extension.processor.AbstractMessageProcessor;
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
        int cpuNum = Runtime.getRuntime().availableProcessors();
        // 定义服务器接受的消息类型以及各类消息对应的处理器
        HttpBootstrap bootstrap = new HttpBootstrap();
        bootstrap.setPort(8080).setThreadNum(cpuNum + 2)
                .setReadBufferSize(1024 * 4)
                .setReadPageSize(16384 * 1024 * 4)
                .setBufferPool(10 * 1024 * 1024, cpuNum + 2, 1024 * 4)
                .pipeline(routeHandle)
                .wrapProcessor(processor -> new AbstractMessageProcessor<>() {
                    @Override
                    public void process0(AioSession session, Request msg) {
                        processor.process(session, msg);
                    }

                    @Override
                    public void stateEvent0(AioSession session, StateMachineEnum stateMachineEnum, Throwable throwable) {
                        processor.stateEvent(session, stateMachineEnum, throwable);
                    }
                }).start();
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
