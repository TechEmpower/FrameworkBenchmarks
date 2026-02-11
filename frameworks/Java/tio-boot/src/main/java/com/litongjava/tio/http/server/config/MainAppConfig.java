package com.litongjava.tio.http.server.config;

import com.litongjava.context.BootConfiguration;
import com.litongjava.tio.boot.server.TioBootServer;
import com.litongjava.tio.http.server.handler.CacheHandler;
import com.litongjava.tio.http.server.handler.DbHandler;
import com.litongjava.tio.http.server.handler.JsonHandler;
import com.litongjava.tio.http.server.handler.PlaintextHandler;
import com.litongjava.tio.http.server.router.HttpRequestRouter;
import com.litongjava.tio.utils.environment.EnvUtils;

public class MainAppConfig implements BootConfiguration {

  @Override
  public void config() throws Exception {

    boolean db = EnvUtils.getBoolean("db", true);
    if (db) {
      try {
        new MysqlDbConfig().init();
      } catch (Exception e) {
        e.printStackTrace();
      }
    }
    // start enjoy and ehcache
    try {
      new EnjoyEngineConfig().engine();
      new EhCachePluginConfig().ehCachePlugin();
    } catch (Exception e) {
      e.printStackTrace();
    }

    // add route
    JsonHandler jsonHanlder = new JsonHandler();

    PlaintextHandler plaintextHandler = new PlaintextHandler();
    TioBootServer server = TioBootServer.me();
    HttpRequestRouter requestRouter = server.getRequestRouter();
    if (requestRouter != null) {
      requestRouter.add("/plaintext", plaintextHandler);
      requestRouter.add("/json", jsonHanlder);

      DbHandler dbQueryController = new DbHandler();
      requestRouter.add("/db", dbQueryController::db);
      requestRouter.add("/queries", dbQueryController::queries);
      requestRouter.add("/updates", dbQueryController::updates);
      requestRouter.add("/fortunes", dbQueryController::fortunes);

      CacheHandler cacheController = new CacheHandler();
      requestRouter.add("/cachedQuery", cacheController);
    }
  }
}
