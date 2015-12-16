package com.techempower.beyondj.action;

import net.sourceforge.stripes.action.*;
import org.stripesrest.JsonBuilder;
import org.stripesrest.JsonResolution;
import com.techempower.beyondj.Message;
import javax.servlet.http.HttpServletResponse;
import java.util.HashMap;
import java.util.Map;

@UrlBinding("/perf/hello/{_eventName}")
public class HelloActionBean extends BaseActionBean {

    public static final String CONTENT_LENGTH = "Content-Length";

    @HandlesEvent(JSON)
    @DefaultHandler
    public Resolution json() {
        Message message = new Message(HELLO_WORLD);

        JsonBuilder builder = new JsonBuilder(message);
        String rawJsonText = builder.build();
        Map<String,String> headers = new HashMap<>();
        headers.put(CONTENT_LENGTH,String.valueOf(rawJsonText.getBytes().length));
        setResponseHeaders(headers);
        return new JsonResolution(rawJsonText);
    }

    @HandlesEvent(PLAINTEXT)
    public Resolution plaintext() {
        return new StreamingResolution(TEXT_PLAIN) {
            public void stream(final HttpServletResponse response) {
                try {
                    Map<String,String> headers = new HashMap<>();
                    headers.put(CONTENT_LENGTH,String.valueOf(HELLO_WORLD.getBytes().length));
                    setResponseHeaders(headers);
                    response.getWriter().write(HELLO_WORLD);
                } catch (Exception e) {
                    //do nothing
                }
            }
        };
    }

    public static final String PLAINTEXT = "plaintext";
    public static final String JSON = "json";
    private static final String HELLO_WORLD = "Hello, World!";
    private static final String TEXT_PLAIN = "text/plain";
}

