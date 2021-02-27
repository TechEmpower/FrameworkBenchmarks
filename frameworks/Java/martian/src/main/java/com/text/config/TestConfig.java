package com.text.config;

import com.mars.common.base.config.MarsConfig;
<<<<<<< HEAD
import com.mars.common.base.config.model.ThreadPoolConfig;
=======
>>>>>>> a5bad622429f14f13d872589d7054aefaa75002d

public class TestConfig extends MarsConfig {

    @Override
    public int port() {
        return 8080;
    }

<<<<<<< HEAD
    @Override
    public ThreadPoolConfig threadPoolConfig() {
        ThreadPoolConfig threadPoolConfig = new ThreadPoolConfig();
        threadPoolConfig.setCorePoolSize(2);
        threadPoolConfig.setMaxPoolSize(10000000);
        threadPoolConfig.setKeepAliveTime(20);
        threadPoolConfig.setBackLog(2000);
        return threadPoolConfig;
    }
=======
>>>>>>> a5bad622429f14f13d872589d7054aefaa75002d
}
