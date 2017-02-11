package com.networknt.techempower.handler;

import com.networknt.techempower.Helper;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.util.Headers;
import io.undertow.util.HttpString;

import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.Map;
import org.apache.commons.lang3.StringEscapeUtils;

public class PlaintextGetHandler implements HttpHandler {
    private static final ByteBuffer buffer;
    private static final String MESSAGE = "Hello, World!";

    static {
        buffer = ByteBuffer.allocateDirect(MESSAGE.length());
        try {
            buffer.put(MESSAGE.getBytes("US-ASCII"));
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        buffer.flip();
    }

    @Override
    public void handleRequest(HttpServerExchange exchange) throws Exception {
        exchange.getResponseHeaders().put(
                Headers.CONTENT_TYPE, "text/plain");
        exchange.getResponseSender().send(buffer.duplicate());
    }
}
