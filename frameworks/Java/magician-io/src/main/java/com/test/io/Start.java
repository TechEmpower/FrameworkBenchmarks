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

            MagicianConfig magicianConfig = new MagicianConfig();
            magicianConfig.setNumberOfPorts(1);
            magicianConfig.setBossThreads(3);
            magicianConfig.setWorkThreads(5);


            HttpServer httpServer = Magician.createHttp()
                    .scan("com.test.io")
                    .setConfig(magicianConfig);

            httpServer.bind(8080);

        } catch (Exception e){
            logger.error("启动服务出现异常", e);
        }
    }
}
