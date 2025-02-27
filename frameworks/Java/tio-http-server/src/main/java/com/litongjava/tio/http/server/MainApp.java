package com.litongjava.tio.http.server;

import com.litongjava.tio.http.common.HttpConfig;
import com.litongjava.tio.http.common.handler.ITioHttpRequestHandler;
import com.litongjava.tio.http.server.config.EhCachePluginConfig;
import com.litongjava.tio.http.server.config.EnjoyEngineConfig;
import com.litongjava.tio.http.server.config.MysqlDbConfig;
import com.litongjava.tio.http.server.controller.CacheController;
import com.litongjava.tio.http.server.controller.DbController;
import com.litongjava.tio.http.server.controller.IndexController;
import com.litongjava.tio.http.server.handler.DefaultHttpRequestDispatcher;
import com.litongjava.tio.http.server.router.DefaultHttpRequestRouter;
import com.litongjava.tio.http.server.router.HttpRequestRouter;
import com.litongjava.tio.server.ServerTioConfig;
import com.litongjava.tio.utils.environment.EnvUtils;

public class MainApp {

  public static void main(String[] args) {
    long start = System.currentTimeMillis();
    EnvUtils.buildCmdArgsMap(args);
    EnvUtils.load();
    // add route
    IndexController controller = new IndexController();

    HttpRequestRouter simpleHttpRoutes = new DefaultHttpRequestRouter();
    simpleHttpRoutes.add("/", controller::index);
    simpleHttpRoutes.add("/plaintext", controller::plaintext);
    simpleHttpRoutes.add("/json", controller::json);

    DbController dbQueryController = new DbController();
    simpleHttpRoutes.add("/db", dbQueryController::db);
    simpleHttpRoutes.add("/queries", dbQueryController::queries);
    simpleHttpRoutes.add("/updates", dbQueryController::updates);
    simpleHttpRoutes.add("/fortunes", dbQueryController::fortunes);

    CacheController cacheController = new CacheController();
    simpleHttpRoutes.add("/cachedQuery", cacheController::cachedQuery);

    // config server
    HttpConfig httpConfig = new HttpConfig(8080, null, null, null);
    httpConfig.setUseSession(false);
    httpConfig.setWelcomeFile(null);
    httpConfig.setCheckHost(false);
    httpConfig.setCompatible1_0(false);

    ITioHttpRequestHandler requestHandler = new DefaultHttpRequestDispatcher(httpConfig, simpleHttpRoutes);
    HttpServerStarter httpServerStarter = new HttpServerStarter(httpConfig, requestHandler);
    ServerTioConfig serverTioConfig = httpServerStarter.getServerTioConfig();
    // close Heartbeat
    serverTioConfig.setHeartbeatTimeout(0);
    serverTioConfig.statOn = false;
    boolean db = EnvUtils.getBoolean("db", true);
    if (db) {
      try {
        new MysqlDbConfig().init();
      } catch (Exception e) {
        e.printStackTrace();
      }
    }
    // start server
    try {
      new EnjoyEngineConfig().engine();
      new EhCachePluginConfig().ehCachePlugin();
      httpServerStarter.start();
      long end = System.currentTimeMillis();
      System.out.println((end - start) + "ms");
    } catch (Exception e) {
      e.printStackTrace();
      System.exit(1);
    }

  }
}