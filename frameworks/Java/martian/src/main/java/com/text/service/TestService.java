package com.text.service;

import com.mars.common.annotation.bean.MarsBean;
import com.mars.iserver.constant.HttpConstant;
import com.mars.iserver.server.impl.MarsHttpExchange;
import com.mars.iserver.server.model.HttpHeaders;
import com.mars.server.server.request.HttpMarsRequest;
import com.mars.server.server.request.HttpMarsResponse;
import com.sun.org.apache.bcel.internal.generic.FADD;
import com.text.api.vo.MessageVO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Date;

@MarsBean
public class TestService {

    public MessageVO json(HttpMarsResponse response){
        /*
            Because this is a purely front-end and back-end separation framework,
            the response header will include cross-domain by default.
            In order to adapt to the tfb test, these default headers are removed here.
         */
        response.geNativeResponse(MarsHttpExchange.class).getResponseHeaders().clear();

        // Add the header required by tfb
        response.setHeader("Content-Type", HttpConstant.RESPONSE_CONTENT_TYPE);
        response.setHeader("Server","Martian");
        response.setHeader("Date", new Date().toGMTString());


        MessageVO messageVO = new MessageVO();
        messageVO.setMessage("Hello, World!");
        return messageVO;
    }
}
