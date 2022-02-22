package com.test.hserver;

import top.hserver.HServerApplication;
import top.hserver.core.ioc.annotation.HServerBoot;
import top.hserver.core.server.context.ConstConfig;

/**
 * @author hxm
 */
@HServerBoot
public class StartApp {

    public static void main(String[] args) {
        HServerApplication.run(StartApp.class, 8888, args);
    }
}
