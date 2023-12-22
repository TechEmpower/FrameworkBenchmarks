package com.litongjava.tio.http.server.controller;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import com.alibaba.fastjson2.JSON;
import com.jfinal.plugin.activerecord.Db;
import com.jfinal.plugin.activerecord.Record;
import com.jfinal.template.Engine;
import com.jfinal.template.Template;
import com.litongjava.tio.http.common.HeaderName;
import com.litongjava.tio.http.common.HeaderValue;
import com.litongjava.tio.http.common.HttpRequest;
import com.litongjava.tio.http.common.HttpResponse;
import com.litongjava.tio.http.server.model.Fortune;
import com.litongjava.tio.http.server.util.Resps;
import com.litongjava.tio.http.server.utils.BeanConverterUtils;
import com.litongjava.tio.http.server.utils.RandomUtils;
import com.litongjava.tio.utils.cache.ICache;
import com.litongjava.tio.utils.cache.caffeine.CaffeineCache;

public class DbController {

  // @GetMapping("/db")
  public HttpResponse db(HttpRequest request) {
    Integer id = request.getInt("id");
    if (id == null) {
      id = RandomUtils.randomWorldNumber();
    }

    HttpResponse httpResponse = new HttpResponse(request);

    // int id = 11;
    // String sql="SELECT id, randomNumber FROM world WHERE id = ?";

    Record recored = Db.findById("world", id);
    if (recored != null) {
      httpResponse.setBody(JSON.toJSONString(recored.toMap()).getBytes());
    } else {
      httpResponse.setBody("{}".getBytes());
    }

    httpResponse.addHeader(HeaderName.Content_Type, HeaderValue.Content_Type.TEXT_PLAIN_JSON);

    return httpResponse;
  }

  // @GetMapping("/queries")
  public HttpResponse queries(HttpRequest request) {
    String queries = request.getParam("queries");
    List<Map<String, Object>> recordMaps = RandomUtils.randomWorldNumbers()
        // limit
        .limit(RandomUtils.parseQueryCount(queries)) // 限制查询数量
        .mapToObj(id -> Db.findById("world", id)) // 使用 mapToObj 将 int 映射为对象
        .filter(Objects::nonNull) // 过滤掉 null 值
        .map(Record::toMap) // 将每个 Record 对象转换为 Map
        .collect(Collectors.toList()); // 收集到 List

    HttpResponse httpResponse = new HttpResponse(request);
    httpResponse.addHeader(HeaderName.Content_Type, HeaderValue.Content_Type.TEXT_PLAIN_JSON);
    httpResponse.setBody(JSON.toJSONString(recordMaps).getBytes());
    return httpResponse;
  }

//@GetMapping("/updates")
  public HttpResponse updates(HttpRequest request) {
    String queries = request.getParam("queries");

    ICache cache = CaffeineCache.getCache("world");
    cache.clear();
    
    List<Map<String, Object>> updatedRecords = RandomUtils.randomWorldNumbers()// random numbers
        // limit
        .limit(RandomUtils.parseQueryCount(queries))
        // map
        .mapToObj(id -> Db.findById("world", id))
        // not null
        .filter(Objects::nonNull).map(record -> {
          int currentRandomNumber = record.getInt("randomNumber"); // "randomnumber"
          int newRandomNumber;
          do {
            newRandomNumber = RandomUtils.randomWorldNumber();
          } while (newRandomNumber == currentRandomNumber);

          record.set("randomnumber", newRandomNumber);
          Db.update("world", "id", record); // update
          return record;
        })
        // tomap
        .map(Record::toMap)
        // to List
        .collect(Collectors.toList());

    HttpResponse httpResponse = new HttpResponse(request);
    httpResponse.addHeader(HeaderName.Content_Type, HeaderValue.Content_Type.TEXT_PLAIN_JSON);
    httpResponse.setBody(JSON.toJSONString(updatedRecords).getBytes());
    return httpResponse;
  }

//@GetMapping("/fortunes")
  public HttpResponse fortunes(HttpRequest request) throws IllegalAccessException, InstantiationException {
    List<Record> records = Db.find("SELECT * FROM fortune"); // 假设表名为 "fortune"

    List<Fortune> fortunes = new ArrayList<>(records.size());
    for (Record record : records) {

      fortunes.add(BeanConverterUtils.toBean(record.toMap(), Fortune.class));
    }
    // 添加额外的 Fortune
    fortunes.add(new Fortune(0L, "Additional fortune added at request time."));

    // 按照消息排序
    fortunes.sort(Comparator.comparing(Fortune::getMessage));

    Map<String, Object> viewData = new HashMap<>();
    viewData.put("fortunes", fortunes);

    // 转换为 HTML
    Engine engine = Engine.use();
    String filename = "fortunes.html";
    Template template = engine.getTemplate(filename);
    String html = template.renderToString(viewData);

    return Resps.html(request, html);
  }
}
