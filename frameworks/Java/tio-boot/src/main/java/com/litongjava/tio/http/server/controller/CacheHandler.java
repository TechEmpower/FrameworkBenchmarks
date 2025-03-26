package com.litongjava.tio.http.server.controller;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.alibaba.fastjson2.JSON;
import com.litongjava.db.activerecord.Db;
import com.litongjava.db.activerecord.Row;
import com.litongjava.tio.boot.http.TioRequestContext;
import com.litongjava.tio.http.common.HeaderName;
import com.litongjava.tio.http.common.HeaderValue;
import com.litongjava.tio.http.common.HttpRequest;
import com.litongjava.tio.http.common.HttpResponse;
import com.litongjava.tio.http.server.utils.RandomUtils;

public class CacheHandler {
  // private Logger log = LoggerFactory.getLogger(this.getClass());
  String sql = "SELECT id, randomNumber FROM world WHERE id = ?";

  public HttpResponse cachedQuery(HttpRequest request) {
    String queries = request.getParam("queries");

    int queryCount = RandomUtils.parseQueryCount(queries);

    List<Map<String, Object>> recordMaps = new ArrayList<>();

    int[] randomNumbers = RandomUtils.randomWorldNumbers().limit(queryCount).toArray();
    for (int id : randomNumbers) {
      Row row = Db.findFirstByCache("world", id, sql, id);
      if (row != null) {
        recordMaps.add(row.toMap());
      }
    }

    HttpResponse response = TioRequestContext.getResponse();
    response.addHeader(HeaderName.Content_Type, HeaderValue.Content_Type.TEXT_PLAIN_JSON);
    response.setBody(JSON.toJSONBytes(recordMaps));
    return response;
  }
}
