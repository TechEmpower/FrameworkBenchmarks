package com.litongjava.tio.http.server.services;

import java.util.ArrayList;
import java.util.List;

import com.litongjava.model.time.Time;

public class CacheNameService {
  private CacheName demo = new CacheName("world", null, Time.MINUTE_1 * 10);

  public List<CacheName> cacheNames() {
    List<CacheName> list = new ArrayList<>();
    list.add(demo);
    return list;
  }

}
