package com.techempower;

import com.fasterxml.jackson.databind.ObjectMapper;
import io.jooby.hikari.HikariModule;
import io.jooby.json.JacksonModule;
import io.jooby.rocker.RockerModule;

import javax.sql.DataSource;

import static io.jooby.ExecutionMode.EVENT_LOOP;
import static io.jooby.Jooby.runApp;

public class JaxrsApp {
  public static void main(String[] args) {
    runApp(args, EVENT_LOOP, app -> {
      /** JSON: */
      app.install(new JacksonModule());

      /** Database: */
      app.install(new HikariModule());

      /** Template engine: */
      app.install(new RockerModule());

      app.mvc(new Resource(app.require(DataSource.class), app.require(ObjectMapper.class)));
    });
  }
}
