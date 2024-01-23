package com.test.io;

import io.magician.Magician;
import io.magician.common.config.MagicianConfig;
import io.magician.network.HttpServer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class Start {

    private static Logger logger = LoggerFactory.getLogger(Start.class);

    public static void main(String[] args) {
        try {

            HttpServer httpServer = Magician.createHttp()
                    .scan("com.test.io")
                    .setConfig(MagicianConfig.create()
                            .setNumberOfPorts(1)
                            .setBossThreads(3)
                            .setWorkThreads(8)
                            .setCorePoolSize(8)
                            .setMaximumPoolSize(10)
                    );

            httpServer.bind(8080);

        } catch (Exception e) {
            logger.error("启动服务出现异常", e);
        }
    }
}
