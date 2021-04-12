package com.text.config;

import com.mars.common.base.config.MarsConfig;
import com.mars.common.base.config.model.ThreadPoolConfig;

public class TestConfig extends MarsConfig {

    @Override
    public int port() {
        return 8080;
    }



    @Override
    public ThreadPoolConfig threadPoolConfig() {
        ThreadPoolConfig threadPoolConfig = new ThreadPoolConfig();
        threadPoolConfig.setCorePoolSize(200);
        threadPoolConfig.setMaxPoolSize(10000000);
        threadPoolConfig.setKeepAliveTime(20);
        threadPoolConfig.setBackLog(2000);
        return threadPoolConfig;
    }
}
