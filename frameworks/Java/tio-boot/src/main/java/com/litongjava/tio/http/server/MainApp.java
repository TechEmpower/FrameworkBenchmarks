package com.litongjava.tio.http.server;

import com.litongjava.tio.boot.TioApplication;

public class MainApp {

  public static void main(String[] args) {
    long start = System.currentTimeMillis();
    TioApplication.run(MainApp.class, new MainAppConfig(), args);
    long end = System.currentTimeMillis();
    System.out.println((end - start) + "ms");
  }
}