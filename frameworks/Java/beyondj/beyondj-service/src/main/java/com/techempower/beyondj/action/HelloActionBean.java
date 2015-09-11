package com.techempower.beyondj.action;

import net.sourceforge.stripes.action.*;
import org.stripesrest.JsonResolution;
import com.techempower.beyondj.Message;
import javax.servlet.http.HttpServletResponse;

@UrlBinding("/perf/hello/{_eventName}")
public class HelloActionBean extends BaseActionBean {

    @HandlesEvent("json")
    @DefaultHandler
    public Resolution json() {
        Message message = new Message("Hello, World!");
        setResponseDate();
        return new JsonResolution(message);
    }

    @HandlesEvent("plaintext")
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

    private static final String HELLO_WORLD = "Hello, World!";
    private static final String TEXT_PLAIN = "text/plain";
}
