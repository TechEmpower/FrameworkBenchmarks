package com.litongjava.tio.http.server;

import java.io.IOException;

import com.litongjava.tio.http.common.HttpConfig;
import com.litongjava.tio.http.common.handler.HttpRequestHandler;
import com.litongjava.tio.http.server.config.CaffeineCacheConfig;
import com.litongjava.tio.http.server.config.EnjoyEngineConfig;
import com.litongjava.tio.http.server.config.MysqlDbConfig;
import com.litongjava.tio.http.server.controller.CacheController;
import com.litongjava.tio.http.server.controller.DbController;
import com.litongjava.tio.http.server.controller.IndexController;
import com.litongjava.tio.http.server.handler.HttpRoutes;
import com.litongjava.tio.http.server.handler.SimpleHttpDispahterHanlder;
import com.litongjava.tio.http.server.handler.SimpleHttpRoutes;
import com.litongjava.tio.server.ServerTioConfig;
import com.litongjava.tio.utils.enviorment.EnviormentUtils;

public class MainApp {

  public static void main(String[] args) throws IOException {
    EnviormentUtils.buildCmdArgsMap(args);
    // add route
    IndexController controller = new IndexController();
    HttpRoutes simpleHttpRoutes = new SimpleHttpRoutes();
    simpleHttpRoutes.add("/", controller::index);
    simpleHttpRoutes.add("/plaintext", controller::plaintext);
    simpleHttpRoutes.add("/json", controller::json);

    DbController dbQueryController = new DbController();
    simpleHttpRoutes.add("/db", dbQueryController::db);
    simpleHttpRoutes.add("/queries", dbQueryController::queries);
    simpleHttpRoutes.add("/updates", dbQueryController::updates);
    simpleHttpRoutes.add("/fortunes", dbQueryController::fortunes);

    CacheController cacheController = new CacheController();
    simpleHttpRoutes.add("/cacheQuery", cacheController::cacheQuery);
    simpleHttpRoutes.add("/cacheList", cacheController::cacheList);

    // config server
    HttpConfig httpConfig = new HttpConfig(8080, null, null, null);
    httpConfig.setUseSession(false);
    httpConfig.setWelcomeFile(null);
    httpConfig.setCheckHost(false);
    httpConfig.setCompatible1_0(false);

    HttpRequestHandler requestHandler = new SimpleHttpDispahterHanlder(httpConfig, simpleHttpRoutes);
    HttpServerStarter httpServerStarter = new HttpServerStarter(httpConfig, requestHandler);
    ServerTioConfig serverTioConfig = httpServerStarter.getServerTioConfig();
    // close Heartbeat
    serverTioConfig.setHeartbeatTimeout(0);
    serverTioConfig.statOn = false;
    // start server
    httpServerStarter.start();

    new MysqlDbConfig().init();
    new EnjoyEngineConfig().engine();
    new CaffeineCacheConfig().register();

  }
}