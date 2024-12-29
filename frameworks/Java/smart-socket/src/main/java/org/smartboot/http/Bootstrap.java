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
import tech.smartboot.feat.core.server.HttpRequest;
import tech.smartboot.feat.core.server.HttpResponse;
import tech.smartboot.feat.core.server.HttpServer;
import tech.smartboot.feat.core.server.HttpServerHandler;
import tech.smartboot.feat.core.server.handler.HttpRouteHandler;

import javax.sql.DataSource;

public class Bootstrap {
    static byte[] body = "Hello, World!".getBytes();

    public static void main(String[] args) {
        int cpuNum = Runtime.getRuntime().availableProcessors();
        // 定义服务器接受的消息类型以及各类消息对应的处理器
        HttpServer bootstrap = new HttpServer();
        bootstrap.configuration()
                .threadNum(cpuNum + 1)
                .headerLimiter(0)
                .readBufferSize(1024 * 4)
                .writeBufferSize(1024 * 4);
        bootstrap.httpHandler(new HttpServerHandler() {
            @Override
            public void handle(HttpRequest request, HttpResponse response) throws Throwable {
                if ("/plaintext".equals(request.getRequestURI())) {
                    response.setContentLength(body.length);
                    response.setContentType("text/plain; charset=UTF-8");
                    response.write(body);
                } else if ("/json".equals(request.getRequestURI())) {
                    response.setContentType("application/json");
                    JsonUtil.writeJsonBytes(response, new Message("Hello, World!"));
                }
            }
        }).setPort(8080).start();
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
