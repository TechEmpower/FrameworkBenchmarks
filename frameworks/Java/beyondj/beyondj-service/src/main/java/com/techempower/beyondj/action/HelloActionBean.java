package com.techempower.beyondj.action;

import net.sourceforge.stripes.action.*;
import org.stripesrest.JsonResolution;
import com.techempower.beyondj.Message;
import javax.servlet.http.HttpServletResponse;

@UrlBinding("/perf/hello/{_eventName}")
public class HelloActionBean extends BaseActionBean {

    @HandlesEvent(JSON)
    @DefaultHandler
    public Resolution json() {
        Message message = new Message(HELLO_WORLD);
        setResponseDate();
        return new JsonResolution(message);
    }

    @HandlesEvent(PLAINTEXT)
    public Resolution plaintext() {
        return new StreamingResolution(TEXT_PLAIN) {
            public void stream(final HttpServletResponse response) {
                try {
                    setResponseDate();
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
