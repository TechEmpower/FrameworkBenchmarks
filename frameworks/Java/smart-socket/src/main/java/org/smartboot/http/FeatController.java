package org.smartboot.http;

import org.smartboot.Message;
import tech.smartboot.feat.cloud.annotation.Controller;
import tech.smartboot.feat.cloud.annotation.RequestMapping;
import tech.smartboot.feat.core.common.HeaderValue;
import tech.smartboot.feat.core.server.HttpResponse;

@Controller
public class FeatController {
    static byte[] body = "Hello, World!".getBytes();

    @RequestMapping("/plaintext")
    public byte[] plaintext(HttpResponse response) {
        response.setContentType(HeaderValue.ContentType.TEXT_PLAIN_UTF8);
        return body;
    }

    @RequestMapping("/json")
    public Message json(HttpResponse response) {
        response.setContentType(HeaderValue.ContentType.APPLICATION_JSON_UTF8);
        return new Message("Hello, World!");
    }
}
