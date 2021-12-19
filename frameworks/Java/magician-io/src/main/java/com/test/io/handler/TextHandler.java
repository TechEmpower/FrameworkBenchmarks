package com.test.io.handler;

import io.magician.tcp.codec.impl.http.request.MagicianRequest;
import io.magician.tcp.handler.MagicianHandler;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

public class TextHandler implements MagicianHandler<MagicianRequest> {

    private SimpleDateFormat simpleDateFormat = new SimpleDateFormat("E, dd MMM yyyy H:m:s z", Locale.US);

    @Override
    public void request(MagicianRequest magicianRequest) {
        String str = simpleDateFormat.format(new Date());

        magicianRequest.getResponse()
                .setResponseHeader("Server","magician")
                .setResponseHeader("Date", str)
                .sendText(200, "Hello, World!");
    }
}
