package com.litongjava.tio.http.server;

import java.lang.Thread.Builder.OfVirtual;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;

import com.litongjava.tio.boot.TioApplication;
import com.litongjava.tio.boot.server.TioBootServer;
import com.litongjava.tio.http.server.config.MainAppConfig;

public class MainApp {

  public static void main(String[] args) {
    long start = System.currentTimeMillis();

    // 1. 虚拟线程工厂（用于 work 线程）
    OfVirtual ofVirtual = Thread.ofVirtual();
    ThreadFactory workTf = ofVirtual.name("t-io-v-", 1).factory();

    // 2. 设置 readWorkers 使用的线程工厂
    TioBootServer server = TioBootServer.me();
    server.setWorkThreadFactory(workTf);

    // 3. 创建业务虚拟线程 Executor（每任务一个虚拟线程）
    ThreadFactory bizTf = Thread.ofVirtual().name("t-biz-v-", 0).factory();

    ExecutorService bizExecutor = Executors.newThreadPerTaskExecutor(bizTf);

    server.setBizExecutor(bizExecutor);

    TioApplication.run(MainApp.class, new MainAppConfig(), args);
    long end = System.currentTimeMillis();
    System.out.println((end - start) + "ms");
  }
}