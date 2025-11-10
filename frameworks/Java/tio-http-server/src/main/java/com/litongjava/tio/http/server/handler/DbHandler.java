package com.litongjava.tio.http.server.handler;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import com.alibaba.fastjson2.JSON;
import com.jfinal.template.Engine;
import com.jfinal.template.Template;
import com.litongjava.db.activerecord.Db;
import com.litongjava.db.activerecord.Row;
import com.litongjava.ehcache.EhCacheKit;
import com.litongjava.tio.http.common.HeaderName;
import com.litongjava.tio.http.common.HeaderValue;
import com.litongjava.tio.http.common.HttpRequest;
import com.litongjava.tio.http.common.HttpResponse;
import com.litongjava.tio.http.server.model.Fortune;
import com.litongjava.tio.http.server.util.Resps;
import com.litongjava.tio.http.server.utils.RandomUtils;

public class DbHandler {

  public HttpResponse db(HttpRequest request) {
    Integer id = request.getInt("id");
    if (id == null) {
      id = RandomUtils.randomWorldNumber();
    }

    //System.out.println("id:" + id);
    HttpResponse httpResponse = new HttpResponse(request);

    // int id = 11;
    // String sql="SELECT id, randomNumber FROM world WHERE id = ?";

    Row recored = Db.findById("world", id);
    if (recored != null) {
      httpResponse.setBody(JSON.toJSONBytes(recored.toMap()));
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
        .map(Row::toMap) // 将每个 Record 对象转换为 Map
        .collect(Collectors.toList()); // 收集到 List

    HttpResponse httpResponse = new HttpResponse(request);
    httpResponse.addHeader(HeaderName.Content_Type, HeaderValue.Content_Type.TEXT_PLAIN_JSON);
    httpResponse.setBody(JSON.toJSONBytes(recordMaps));
    return httpResponse;
  }

  //@GetMapping("/updates")
  public HttpResponse updates(HttpRequest request) {
    String queries = request.getParam("queries");

    EhCacheKit.removeAll("world");

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
        .map(Row::toMap)
        // to List
        .collect(Collectors.toList());

    HttpResponse httpResponse = new HttpResponse(request);
    httpResponse.addHeader(HeaderName.Content_Type, HeaderValue.Content_Type.TEXT_PLAIN_JSON);
    httpResponse.setBody(JSON.toJSONBytes(updatedRecords));
    return httpResponse;
  }

  public HttpResponse fortunes(HttpRequest request) throws IllegalAccessException, InstantiationException {
    List<Row> records = Db.find("SELECT * FROM fortune");

    List<Fortune> fortunes = new ArrayList<>(records.size());
    for (Row record : records) {
      fortunes.add(record.toBean(Fortune.class));
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
