package org.smartboot.http;

import org.smartboot.Message;
import tech.smartboot.feat.core.apt.annotation.Controller;
import tech.smartboot.feat.core.apt.annotation.RequestMapping;
import tech.smartboot.feat.core.common.enums.HeaderValueEnum;
import tech.smartboot.feat.core.server.HttpResponse;

@Controller
public class FeatController {
    static byte[] body = "Hello, World!".getBytes();

    @RequestMapping("/plaintext")
    public byte[] plaintext(HttpResponse response) {
        response.setContentType(HeaderValueEnum.ContentType.TEXT_PLAIN_UTF8);
        return body;
    }

    @RequestMapping("/json")
    public Message json(HttpResponse response) {
        response.setContentType(HeaderValueEnum.ContentType.TEXT_PLAIN_UTF8);
        return new Message("Hello, World!");
    }
}
