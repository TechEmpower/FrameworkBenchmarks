package com.litongjava.tio.http.server.controller;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import com.alibaba.fastjson2.JSON;
import com.litongjava.db.activerecord.Db;
import com.litongjava.db.activerecord.Record;
import com.litongjava.tio.http.common.HeaderName;
import com.litongjava.tio.http.common.HeaderValue;
import com.litongjava.tio.http.common.HttpRequest;
import com.litongjava.tio.http.common.HttpResponse;
import com.litongjava.tio.http.server.utils.RandomUtils;

public class CacheController {
  // private Logger log = LoggerFactory.getLogger(this.getClass());

  public HttpResponse cachedQuery(HttpRequest request) {
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
    httpResponse.setBody(JSON.toJSONBytes(recordMaps));
    return httpResponse;

  }

  private Record findByIdWithCache(String tableName, int id) {
    String sql = "SELECT id, randomNumber FROM world WHERE id = ?";
    return Db.findFirstByCache(tableName, id, sql, id);
  }
}
