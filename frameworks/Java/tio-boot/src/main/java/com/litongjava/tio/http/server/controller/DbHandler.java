package com.litongjava.tio.http.server.controller;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.alibaba.fastjson2.JSON;
import com.jfinal.template.Engine;
import com.jfinal.template.Template;
import com.litongjava.db.activerecord.Db;
import com.litongjava.db.activerecord.Row;
import com.litongjava.ehcache.EhCacheKit;
import com.litongjava.tio.boot.http.TioRequestContext;
import com.litongjava.tio.http.common.HeaderName;
import com.litongjava.tio.http.common.HeaderValue;
import com.litongjava.tio.http.common.HttpRequest;
import com.litongjava.tio.http.common.HttpResponse;
import com.litongjava.tio.http.server.model.Fortune;
import com.litongjava.tio.http.server.util.Resps;
import com.litongjava.tio.http.server.utils.RandomUtils;

public class DbHandler {
  private Engine engine = Engine.use();
  private String filename = "fortunes.html";
  private Template template = engine.getTemplate(filename);
  private static final byte[] bytes = "{}".getBytes();

  public HttpResponse db(HttpRequest request) {
    Integer id = request.getInt("id");
    if (id == null) {
      id = RandomUtils.randomWorldNumber();
    }

    //System.out.println("id:" + id);
    HttpResponse httpResponse = TioRequestContext.getResponse();

    // int id = 11;
    // String sql="SELECT id, randomNumber FROM world WHERE id = ?";

    Row recored = Db.findById("world", id);
    if (recored != null) {
      httpResponse.setBody(JSON.toJSONBytes(recored.toMap()));
    } else {
      httpResponse.setBody(bytes);
    }

    httpResponse.addHeader(HeaderName.Content_Type, HeaderValue.Content_Type.TEXT_PLAIN_JSON);

    return httpResponse;
  }

  // @GetMapping("/queries")
  public HttpResponse queries(HttpRequest request) {
    String queries = request.getParam("queries");
    int queryCount = RandomUtils.parseQueryCount(queries);
    int[] randomNumbers = RandomUtils.randomWorldNumbers().limit(queryCount).toArray();

    List<Map<String, Object>> recordMaps = new ArrayList<>();

    for (int id : randomNumbers) {
      Row row = Db.findById("world", id);
      if (row != null) {
        recordMaps.add(row.toMap());
      }
    }

    HttpResponse httpResponse = TioRequestContext.getResponse();
    httpResponse.addHeader(HeaderName.Content_Type, HeaderValue.Content_Type.TEXT_PLAIN_JSON);
    httpResponse.setBody(JSON.toJSONBytes(recordMaps));
    return httpResponse;
  }

  //@GetMapping("/updates")
  public HttpResponse updates(HttpRequest request) {
    String queries = request.getParam("queries");

    EhCacheKit.removeAll("world");

    int queryCount = RandomUtils.parseQueryCount(queries);
    int[] randomNumbers = RandomUtils.randomWorldNumbers().limit(queryCount).toArray();
    List<Map<String, Object>> updatedRecords = new ArrayList<>();

    for (int id : randomNumbers) {
      Row row = Db.findById("world", id);
      if (row != null) {
        int currentRandomNumber = row.getInt("randomNumber"); // "randomnumber"
        int newRandomNumber;
        do {
          newRandomNumber = RandomUtils.randomWorldNumber();
        } while (newRandomNumber == currentRandomNumber);

        row.set("randomnumber", newRandomNumber);
        Db.update("world", "id", row); // update
        updatedRecords.add(row.toMap());
      }
    }

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
    String html = template.renderToString(viewData);
    HttpResponse httpResponse = TioRequestContext.getResponse();
    return Resps.html(httpResponse, html);
  }
}
