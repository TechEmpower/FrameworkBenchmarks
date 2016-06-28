package com.techempower.beyondj.action;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.techempower.beyondj.Message;
import net.sourceforge.stripes.action.*;
import org.stripesrest.JsonResolution;

import javax.servlet.http.HttpServletResponse;
import java.util.HashMap;
import java.util.Map;

@UrlBinding("/perf/hello/{_eventName}")
public class HelloActionBean extends BaseActionBean {

    public static final String CONTENT_LENGTH = "Content-Length";

    @HandlesEvent(JSON)
    @DefaultHandler
    public Resolution json() throws Exception{
        Message message = new Message(HELLO_WORLD);

        Gson gson = new GsonBuilder()
                .create();

        String rawJsonText = gson.toJson(message);
        Map<String,String> headers = new HashMap<>();
        headers.put(CONTENT_LENGTH,String.valueOf(rawJsonText.getBytes().length));
        getContext().getResponse().setCharacterEncoding("UTF-8");
        getContext().getRequest().setCharacterEncoding("UTF-8");
        getContext().getResponse().setContentType("application/json");
        setResponseHeaders(headers);
        return new JsonResolution(rawJsonText);
    }

    @HandlesEvent(PLAINTEXT)
    public Resolution plaintext() throws Exception{
        return new StreamingResolution(TEXT_PLAIN) {
            public void stream(final HttpServletResponse response) {
                try {
                    Map<String,String> headers = new HashMap<>();
                    headers.put(CONTENT_LENGTH,String.valueOf(HELLO_WORLD.getBytes().length));
                    getContext().getResponse().setCharacterEncoding("UTF-8");
                    getContext().getRequest().setCharacterEncoding("UTF-8");
                    getContext().getResponse().setContentType("text/html");
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

