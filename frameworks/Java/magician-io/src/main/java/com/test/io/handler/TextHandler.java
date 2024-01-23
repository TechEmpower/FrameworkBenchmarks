package com.test.io.handler;

import io.magician.application.request.MagicianRequest;
import io.magician.application.request.MagicianResponse;
import io.magician.common.annotation.HttpHandler;
import io.magician.network.handler.HttpBaseHandler;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

@HttpHandler(path = "plaintext")
public class TextHandler implements HttpBaseHandler {

    private SimpleDateFormat simpleDateFormat = new SimpleDateFormat("E, dd MMM yyyy H:m:s z", Locale.US);


    @Override
    public void request(MagicianRequest magicianRequest, MagicianResponse magicianResponse) {
        String str = simpleDateFormat.format(new Date());

        try {
            magicianRequest.getResponse()
                    .setResponseHeader("Server","magician")
                    .setResponseHeader("Date", str)
                    .sendText("Hello, World!");
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
