package com.test.io;

import com.test.io.handler.JsonHandler;
import com.test.io.handler.TextHandler;
import io.magician.Magician;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Start {

    private static Logger logger = LoggerFactory.getLogger(Start.class);

    public static void main(String[] args) {
        try {

            Magician.createTCPServer()
                    .handler("/json", new JsonHandler())
                    .handler("/plaintext", new TextHandler())
                    .bind(8080, 10000);

        } catch (Exception e){
            logger.error("启动服务出现异常", e);
        }
    }
}
