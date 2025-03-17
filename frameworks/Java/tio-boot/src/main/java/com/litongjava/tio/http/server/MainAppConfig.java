package com.litongjava.tio.http.server;

import com.litongjava.context.BootConfiguration;
import com.litongjava.tio.boot.server.TioBootServer;
import com.litongjava.tio.http.server.config.EhCachePluginConfig;
import com.litongjava.tio.http.server.config.EnjoyEngineConfig;
import com.litongjava.tio.http.server.config.MysqlDbConfig;
import com.litongjava.tio.http.server.controller.CacheHandler;
import com.litongjava.tio.http.server.controller.DbHandler;
import com.litongjava.tio.http.server.controller.IndexHandler;
import com.litongjava.tio.http.server.router.HttpRequestRouter;
import com.litongjava.tio.utils.environment.EnvUtils;

public class MainAppConfig implements BootConfiguration {

  @Override
  public void config() throws Exception {
    // add route
    IndexHandler controller = new IndexHandler();

    TioBootServer server = TioBootServer.me();
    HttpRequestRouter requestRouter = server.getRequestRouter();

    requestRouter.add("/", controller::index);
    requestRouter.add("/plaintext", controller::plaintext);
    requestRouter.add("/json", controller::json);

    DbHandler dbQueryController = new DbHandler();
    requestRouter.add("/db", dbQueryController::db);
    requestRouter.add("/queries", dbQueryController::queries);
    requestRouter.add("/updates", dbQueryController::updates);
    requestRouter.add("/fortunes", dbQueryController::fortunes);

    CacheHandler cacheController = new CacheHandler();
    requestRouter.add("/cachedQuery", cacheController::cachedQuery);

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
  }

}
