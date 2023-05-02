package pmg.controller;

import org.noear.solon.annotation.Controller;
import org.noear.solon.annotation.Get;
import org.noear.solon.annotation.Mapping;
import pmg.model.Message;

/**
 * @author noear
 * @version V1.0
 */
@Controller
public class HelloController {
    @Get
    @Mapping("plaintext")
    public String plaintext() {
        return "Hello, World!";
    }

    @Get
    @Mapping("json")
    public Message json() {
        return new Message("Hello, World!");
    }
}
