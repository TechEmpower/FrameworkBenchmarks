package com.text.service;

import com.mars.common.annotation.bean.MarsBean;
import com.mars.iserver.server.impl.MarsHttpExchange;
import com.mars.iserver.server.model.HttpHeaders;
import com.mars.server.server.request.HttpMarsRequest;
import com.mars.server.server.request.HttpMarsResponse;
import com.text.api.vo.MessageVO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Date;

@MarsBean
public class TestService {

    private Logger logger = LoggerFactory.getLogger(TestService.class);

    public MessageVO json(HttpMarsResponse response){
        MessageVO messageVO = new MessageVO();
        messageVO.setMessage("Hello, World!");

        response.setHeader("Server","Martian");
        response.setHeader("Date", "Wed, "+new Date().toGMTString());

        return messageVO;
    }
}
