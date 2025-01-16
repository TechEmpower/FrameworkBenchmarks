/*
 * Copyright (c) 2018, org.smartboot. All rights reserved.
 * project name: smart-socket
 * file name: HttpBootstrap.java
 * Date: 2018-01-28
 * Author: sandao
 */

package org.smartboot.http;

import org.smartboot.Message;
import tech.smartboot.feat.core.Feat;
import tech.smartboot.feat.core.common.enums.HeaderValueEnum;
import tech.smartboot.feat.core.server.HttpHandler;
import tech.smartboot.feat.core.server.HttpRequest;
import tech.smartboot.feat.core.server.HttpResponse;

public class Bootstrap {
    static byte[] body = "Hello, World!".getBytes();

    public static void main(String[] args) {
        int cpuNum = Runtime.getRuntime().availableProcessors();
        // 定义服务器接受的消息类型以及各类消息对应的处理器
        Feat.createHttpServer(options -> {
            options.threadNum(cpuNum + 1)
                    .headerLimiter(0)
                    .readBufferSize(1024 * 4)
                    .writeBufferSize(1024 * 4);
        }).httpHandler(request -> {
            HttpResponse response = request.getResponse();
            if ("/plaintext".equals(request.getRequestURI())) {
                response.setContentLength(body.length);
                response.setContentType(HeaderValueEnum.ContentType.TEXT_PLAIN_UTF8);
                response.write(body);
            } else if ("/json".equals(request.getRequestURI())) {
                response.setContentType("application/json");
                JsonUtil.writeJsonBytes(response, new Message("Hello, World!"));
            }
        }).listen(8080);
    }

//    private static void initDB(HttpRouteHandler routeHandle) {
//        try {
//            Class.forName("org.postgresql.Driver");
//        } catch (ClassNotFoundException e) {
//            e.printStackTrace();
//        }
//        HikariConfig config = new HikariConfig();
//        config.setJdbcUrl("jdbc:postgresql://tfb-database:5432/hello_world");
//        config.setUsername("benchmarkdbuser");
//        config.setPassword("benchmarkdbpass");
//        config.setMaximumPoolSize(64);
//        config.addDataSourceProperty("cachePrepStmts", "true");
//        config.addDataSourceProperty("prepStmtCacheSize", "250");
//        config.addDataSourceProperty("prepStmtCacheSqlLimit", "2048");
//        DataSource dataSource = new HikariDataSource(config);
//        routeHandle.route("/db", new SingleQueryHandler(dataSource))
//                .route("/queries", new MultipleQueriesHandler(dataSource))
//                .route("/updates", new UpdateHandler(dataSource));
////                .route("/fortunes", new FortunesHandler(dataSource));
//    }
}
