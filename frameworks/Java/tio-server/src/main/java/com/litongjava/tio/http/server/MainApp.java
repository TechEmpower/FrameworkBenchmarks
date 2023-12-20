package com.litongjava.tio.http.server;

import java.io.IOException;

import com.litongjava.tio.http.common.HttpConfig;
import com.litongjava.tio.http.common.handler.HttpRequestHandler;
import com.litongjava.tio.http.server.controller.IndexController;
import com.litongjava.tio.http.server.handler.HttpRoutes;
import com.litongjava.tio.http.server.handler.SimpleHttpDispahterHanlder;
import com.litongjava.tio.http.server.handler.SimpleHttpRoutes;
import com.litongjava.tio.server.ServerTioConfig;

public class MainApp {

  public static void main(String[] args) throws IOException {
    // add route
    IndexController controller = new IndexController();
    HttpRoutes simpleHttpRoutes = new SimpleHttpRoutes();
    simpleHttpRoutes.add("/", controller::index);
    simpleHttpRoutes.add("/plaintext", controller::plaintext);
    simpleHttpRoutes.add("/json", controller::json);

    // config server
    HttpConfig httpConfig = new HttpConfig(8080, null, null, null);
    httpConfig.setUseSession(false);
    httpConfig.setWelcomeFile(null);
    httpConfig.setCheckHost(false);
    httpConfig.setCompatible1_0(false);

    HttpRequestHandler requestHandler = new SimpleHttpDispahterHanlder(httpConfig, simpleHttpRoutes);
    HttpServerStarter httpServerStarter = new HttpServerStarter(httpConfig, requestHandler);
    ServerTioConfig serverTioConfig = httpServerStarter.getServerTioConfig();
    // close Heartbeat
    serverTioConfig.setHeartbeatTimeout(0);
    serverTioConfig.statOn = false;
    // start server
    httpServerStarter.start();
  }

}