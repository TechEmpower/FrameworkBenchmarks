  package com.litongjava.tio.http.server.config;

import com.litongjava.ehcache.EhCachePlugin;

public class EhCachePluginConfig {

  public EhCachePlugin ehCachePlugin() {
    EhCachePlugin ehCachePlugin = new EhCachePlugin();
    ehCachePlugin.start();
    return ehCachePlugin;
  }
}
