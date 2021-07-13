package com.test.hserver.controller;

import com.test.hserver.bean.Message;
import top.hserver.core.ioc.annotation.Controller;
import top.hserver.core.ioc.annotation.GET;

/**
 * @author hxm
 */
@Controller
public class TestController {

    @GET("/json")
    public Message json() {
        return new Message();
    }

    @GET("/plaintext")
    public String plaintext() {
        return "Hello, World!";
    }
}
