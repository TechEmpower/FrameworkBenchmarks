package com.techempower;

import io.jooby.hikari.HikariModule;
import io.jooby.rocker.RockerModule;

import javax.sql.DataSource;

import static io.jooby.ExecutionMode.EVENT_LOOP;
import static io.jooby.Jooby.runApp;

public class MvcApp {
  public static void main(String[] args) {
    runApp(args, EVENT_LOOP, app -> {
      /** Database: */
      app.install(new HikariModule());

      /** Template engine: */
      app.install(new RockerModule());

      app.mvc(new Resource_(app.require(DataSource.class)));
    });
  }
}
