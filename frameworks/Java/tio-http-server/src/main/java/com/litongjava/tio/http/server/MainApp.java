package com.litongjava.tio.http.server;

import com.litongjava.tio.http.common.HttpConfig;
import com.litongjava.tio.http.common.handler.HttpRequestHandler;
import com.litongjava.tio.http.server.config.EhCachePluginConfig;
import com.litongjava.tio.http.server.config.EnjoyEngineConfig;
import com.litongjava.tio.http.server.config.MysqlDbConfig;
import com.litongjava.tio.http.server.controller.CacheController;
import com.litongjava.tio.http.server.controller.DbController;
import com.litongjava.tio.http.server.controller.IndexController;
import com.litongjava.tio.http.server.handler.HttpRoutes;
import com.litongjava.tio.http.server.handler.SimpleHttpDispatcherHandler;
import com.litongjava.tio.http.server.handler.SimpleHttpRoutes;
import com.litongjava.tio.server.ServerTioConfig;
import com.litongjava.tio.utils.environment.EnvironmentUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MainApp {

  public static void main(String[] args) {
    Logger log = LoggerFactory.getLogger(MainApp.class);
    long start = System.currentTimeMillis();
    EnvironmentUtils.buildCmdArgsMap(args);
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

    // config server
    HttpConfig httpConfig = new HttpConfig(8080, null, null, null);
    httpConfig.setUseSession(false);
    httpConfig.setWelcomeFile(null);
    httpConfig.setCheckHost(false);
    httpConfig.setCompatible1_0(false);

    HttpRequestHandler requestHandler = new SimpleHttpDispatcherHandler(httpConfig, simpleHttpRoutes);
    HttpServerStarter httpServerStarter = new HttpServerStarter(httpConfig, requestHandler);
    ServerTioConfig serverTioConfig = httpServerStarter.getServerTioConfig();
    // close Heartbeat
    serverTioConfig.setHeartbeatTimeout(0);
    serverTioConfig.statOn = false;
    // start server
    try {
      httpServerStarter.start();
      if (!EnvironmentUtils.getBoolean("native")) {
        new MysqlDbConfig().init();
        new EnjoyEngineConfig().engine();
        new EhCachePluginConfig().ehCachePlugin();
      } else {
        log.info("native mode,only start server");
      }
      long end = System.currentTimeMillis();
      System.out.println((end - start) + "ms");
    } catch (Exception e) {
      e.printStackTrace();
      System.exit(1);
    }

  }
}