package com.text.config;

import com.martian.config.MartianConfig;
import io.magician.common.event.EventGroup;

import java.util.concurrent.Executors;

public class TestConfig extends MartianConfig {

    @Override
    public int port() {
        return 8080;
    }

    @Override
    public EventGroup workerEventGroup() {
        return new EventGroup(10, Executors.newCachedThreadPool());
    }
}
