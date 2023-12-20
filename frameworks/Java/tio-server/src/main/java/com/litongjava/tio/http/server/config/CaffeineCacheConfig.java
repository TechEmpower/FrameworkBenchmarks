package com.litongjava.tio.http.server.config;

import java.util.List;

import com.litongjava.tio.http.server.services.CacheName;
import com.litongjava.tio.http.server.services.CacheNameService;
import com.litongjava.tio.utils.cache.caffeine.CaffeineCache;

public class CaffeineCacheConfig {

  public CacheNameService register() {
    CacheNameService cacheNameService = new CacheNameService();
    List<CacheName> names = cacheNameService.cacheNames();
    for (CacheName cacheName : names) {
      CaffeineCache.register(cacheName.getName(), cacheName.getTimeToLiveSeconds(), cacheName.getTimeToIdleSeconds());
    }
    return cacheNameService;
  }
}
