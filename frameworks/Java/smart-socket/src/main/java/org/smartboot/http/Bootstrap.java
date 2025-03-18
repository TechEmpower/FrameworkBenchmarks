/*
 * Copyright (c) 2018, org.smartboot. All rights reserved.
 * project name: smart-socket
 * file name: HttpBootstrap.java
 * Date: 2018-01-28
 * Author: sandao
 */

package org.smartboot.http;


import tech.smartboot.feat.cloud.FeatCloud;

public class Bootstrap {

    public static void main(String[] args) {
        int cpuNum = Runtime.getRuntime().availableProcessors();
        FeatCloud.cloudServer(options -> {
            options.threadNum(cpuNum + 1)
                    .headerLimiter(0)
                    .readBufferSize(1024 * 4)
                    .writeBufferSize(1024 * 4);
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
