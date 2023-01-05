package com.test.hserver;


import cn.hserver.HServerApplication;
import cn.hserver.core.ioc.annotation.HServerBoot;
import cn.hserver.core.server.context.ConstConfig;


/**
 * @author hxm
 */
@HServerBoot
public class StartApp {

    public static void main(String[] args) {
        ConstConfig.bossPool = Runtime.getRuntime().availableProcessors()/2;
        ConstConfig.workerPool = Runtime.getRuntime().availableProcessors();
        HServerApplication.run(StartApp.class, 8888, args);
    }
}
