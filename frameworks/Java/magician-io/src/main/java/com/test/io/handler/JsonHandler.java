package com.test.io.handler;

import com.alibaba.fastjson.JSON;
import com.test.io.vo.MessageVO;
import io.magician.application.request.MagicianRequest;
import io.magician.application.request.MagicianResponse;
import io.magician.common.annotation.HttpHandler;
import io.magician.network.handler.HttpBaseHandler;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

@HttpHandler(path = "/json")
public class JsonHandler implements HttpBaseHandler {

    private SimpleDateFormat simpleDateFormat = new SimpleDateFormat("E, dd MMM yyyy H:m:s z", Locale.US);


    @Override
    public void request(MagicianRequest magicianRequest, MagicianResponse magicianResponse) {
        String str = simpleDateFormat.format(new Date());

        MessageVO messageVO = new MessageVO();
        messageVO.setMessage("Hello, World!");

        try {
            magicianResponse
                    .setResponseHeader("Server","magician")
                    .setResponseHeader("Date", str)
                    .sendJson(JSON.toJSONString(messageVO));
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
