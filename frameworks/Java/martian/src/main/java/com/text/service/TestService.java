package com.text.service;

import com.mars.aio.constant.HttpConstant;
import com.mars.aio.server.impl.MarsHttpExchange;
import com.mars.common.annotation.bean.MarsBean;
import com.mars.server.server.request.HttpMarsResponse;
import com.text.api.vo.MessageVO;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

@MarsBean
public class TestService {

    private SimpleDateFormat simpleDateFormat = new SimpleDateFormat("E, dd MMM yyyy H:m:s z", Locale.US);

    public MessageVO json(HttpMarsResponse response){
        /*
            Because this is a purely front-end and back-end separation framework,
            the response header will include cross-domain by default.
            In order to adapt to the tfb test, these default headers are removed here.
         */
        response.geNativeResponse(MarsHttpExchange.class).getResponseHeaders().clear();

        // Add the header required by tfb
        String str = simpleDateFormat.format(new Date());

        response.setHeader("Content-Type", HttpConstant.RESPONSE_CONTENT_TYPE);
        response.setHeader("Server","Martian");
        response.setHeader("Date", str);


        MessageVO messageVO = new MessageVO();
        messageVO.setMessage("Hello, World!");
        return messageVO;
    }
}
