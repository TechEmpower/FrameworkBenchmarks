package com.litongjava.tio.http.server.controller;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import com.alibaba.fastjson2.JSON;
import com.jfinal.plugin.activerecord.Db;
import com.jfinal.plugin.activerecord.Record;
import com.litongjava.tio.http.common.HeaderName;
import com.litongjava.tio.http.common.HeaderValue;
import com.litongjava.tio.http.common.HttpRequest;
import com.litongjava.tio.http.common.HttpResponse;
import com.litongjava.tio.http.server.utils.RandomUtils;
import com.litongjava.tio.utils.cache.CacheUtils;
import com.litongjava.tio.utils.cache.FirsthandCreater;
import com.litongjava.tio.utils.cache.ICache;
import com.litongjava.tio.utils.cache.caffeine.CaffeineCache;

public class CacheController {
  // private Logger log = LoggerFactory.getLogger(this.getClass());

  public HttpResponse cacheQuery(HttpRequest request) {
    String queries = request.getParam("queries");
    List<Map<String, Object>> recordMaps = RandomUtils.randomWorldNumbers()
        // limit
        .limit(RandomUtils.parseQueryCount(queries)) // 限制查询数量
        .mapToObj(id -> findByIdWithCache("world", id)) // 使用 mapToObj 将 int 映射为对象
        .filter(Objects::nonNull) // 过滤掉 null 值
        .map(Record::toMap) // 将每个 Record 对象转换为 Map
        .collect(Collectors.toList()); // 收集到 List

    HttpResponse httpResponse = new HttpResponse(request);
    httpResponse.addHeader(HeaderName.Content_Type, HeaderValue.Content_Type.TEXT_PLAIN_JSON);
    httpResponse.setBody(JSON.toJSONString(recordMaps).getBytes());
    return httpResponse;

  }

  private Record findByIdWithCache(String tableName, int id) {

    ICache cache = CaffeineCache.getCache("world");

    // firsthandCreater用户查询数据库
    FirsthandCreater<Record> firsthandCreater = new FirsthandCreater<Record>() {
      @Override
      public Record create() {
        // log.info("select from db:{},id", tableName, id);
        return Db.findById(tableName, id);
      }
    };

    String key = id + "";
    boolean putTempToCacheIfNull = false;
    Record value = CacheUtils.get(cache, key, putTempToCacheIfNull, firsthandCreater);
    return value;
  }

  public HttpResponse cacheList(HttpRequest request) {
    Collection<String> keys = CaffeineCache.getCache("world").keys();

    HttpResponse httpResponse = new HttpResponse(request);
    httpResponse.addHeader(HeaderName.Content_Type, HeaderValue.Content_Type.TEXT_PLAIN_JSON);
    httpResponse.setBody(JSON.toJSONString(keys).getBytes());
    return httpResponse;
  }
}
