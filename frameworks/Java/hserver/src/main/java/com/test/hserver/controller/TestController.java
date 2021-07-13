package com.test.hserver.controller;

import com.test.hserver.bean.Message;
import top.hserver.core.interfaces.HttpResponse;
import top.hserver.core.ioc.annotation.Controller;
import top.hserver.core.ioc.annotation.GET;

import java.util.Date;

/**
 * @author hxm
 */
@Controller
public class TestController {

    @GET("/json")
    public Message json(HttpResponse response) {
        response.setHeader("Date", new Date().toString());
        return new Message();
    }

    @GET("/plaintext")
    public String plaintext(HttpResponse response) {
        response.setHeader("Date", new Date().toString());
        return "Hello, World!";
    }
}
