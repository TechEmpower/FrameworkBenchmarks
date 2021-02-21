package com.text.config;

import com.mars.common.base.config.MarsConfig;

public class TestConfig extends MarsConfig {

    @Override
    public int port() {
        return 8080;
    }

}
