package com.text.api;

import com.magician.web.core.annotation.Route;
import com.text.api.vo.MessageVO;
import io.magician.tcp.codec.impl.http.request.MagicianRequest;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

public class TestApi {

    private SimpleDateFormat simpleDateFormat = new SimpleDateFormat("E, dd MMM yyyy H:m:s z", Locale.US);

    @Route("/json")
    public MessageVO json(MagicianRequest request){

        String str = simpleDateFormat.format(new Date());

        request.getResponse()
                .setResponseHeader("Server","Martian")
                .setResponseHeader("Date", str);

        MessageVO messageVO = new MessageVO();
        messageVO.setMessage("Hello, World!");
        return messageVO;
    }
}
