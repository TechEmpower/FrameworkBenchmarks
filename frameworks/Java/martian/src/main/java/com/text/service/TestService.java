package com.text.service;

import com.mars.common.annotation.bean.MarsBean;
<<<<<<< HEAD
import com.mars.iserver.constant.HttpConstant;
=======
>>>>>>> a5bad622429f14f13d872589d7054aefaa75002d
import com.mars.iserver.server.impl.MarsHttpExchange;
import com.mars.iserver.server.model.HttpHeaders;
import com.mars.server.server.request.HttpMarsRequest;
import com.mars.server.server.request.HttpMarsResponse;
<<<<<<< HEAD
import com.sun.org.apache.bcel.internal.generic.FADD;
=======
>>>>>>> a5bad622429f14f13d872589d7054aefaa75002d
import com.text.api.vo.MessageVO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Date;

@MarsBean
public class TestService {

<<<<<<< HEAD
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
=======
    private Logger logger = LoggerFactory.getLogger(TestService.class);

    public MessageVO json(HttpMarsResponse response){
        MessageVO messageVO = new MessageVO();
        messageVO.setMessage("Hello, World!");

        response.setHeader("Server","Martian");
        response.setHeader("Date", "Wed, "+new Date().toGMTString());

>>>>>>> a5bad622429f14f13d872589d7054aefaa75002d
        return messageVO;
    }
}
