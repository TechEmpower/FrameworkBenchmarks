package com.test.hserver.controller;

import com.test.hserver.bean.Message;
import top.hserver.core.ioc.annotation.Controller;
import top.hserver.core.ioc.annotation.GET;

/**
 * @author hxm
 */
@Controller
public class TestController {

    private static Message message = new Message();

    @GET("/json")
    public Message json() {
        return message;
    }

    @GET("/plaintext")
    public String plaintext() {
        return "Hello, World!";
    }
}
