package com.test.hserver;


import cn.hserver.core.boot.HServerApplication;
import cn.hserver.core.boot.annotation.HServerBoot;

/**
 * @author hxm
 */
@HServerBoot
public class StartApp {

    public static void main(String[] args) {
        HServerApplication.run(StartApp.class, args);
    }
}
