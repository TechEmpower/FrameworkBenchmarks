package com.test.hserver;


import cn.hserver.core.boot.HServerApplication;
import cn.hserver.core.boot.annotation.HServerBoot;
import cn.hserver.mvc.server.WebServer;
import com.test.hserver.controller.TestController;

/**
 * @author hxm
 */
@HServerBoot
public class StartApp {

    public static void main(String[] args) {
        WebServer.router
                .get("/json", TestController::json)
                .get("/plaintext", TestController::plaintext)
                .get("/db", TestController::db)
                .get("/queries", TestController::queries)
                .get("/updates", TestController::updates)
                .get("/fortunes", TestController::fortunes)
        ;
        HServerApplication.run(StartApp.class,  args);
    }
}
