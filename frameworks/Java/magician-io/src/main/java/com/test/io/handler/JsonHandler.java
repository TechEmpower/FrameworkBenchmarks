package com.test.io.handler;

import com.alibaba.fastjson.JSON;
import com.test.io.vo.MessageVO;
import io.magician.tcp.codec.impl.http.request.MagicianRequest;
import io.magician.tcp.handler.MagicianHandler;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

public class JsonHandler implements MagicianHandler<MagicianRequest> {

    private SimpleDateFormat simpleDateFormat = new SimpleDateFormat("E, dd MMM yyyy H:m:s z", Locale.US);

    @Override
    public void request(MagicianRequest magicianRequest) {
        String str = simpleDateFormat.format(new Date());

        MessageVO messageVO = new MessageVO();
        messageVO.setMessage("Hello, World!");

        magicianRequest.getResponse()
                .setResponseHeader("Server","magician")
                .setResponseHeader("Date", str)
                .sendJson(200, JSON.toJSONString(messageVO));
    }
}
