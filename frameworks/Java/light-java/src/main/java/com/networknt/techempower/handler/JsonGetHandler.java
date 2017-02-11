package com.networknt.techempower.handler;

import com.dslplatform.json.JsonWriter;
import com.dslplatform.json.MapConverter;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.networknt.config.Config;
import com.networknt.techempower.Helper;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.util.Headers;
import io.undertow.util.HttpString;

import java.nio.ByteBuffer;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import org.apache.commons.lang3.StringEscapeUtils;

public class JsonGetHandler implements HttpHandler {
    private final ObjectMapper objectMapper = Config.getInstance().getMapper();
    private JsonWriter writer = new JsonWriter();

    @Override
    public void handleRequest(HttpServerExchange exchange) throws Exception {
        /*
        // 946188.45 2.63
        exchange.getResponseHeaders().put(
                Headers.CONTENT_TYPE, "application/json");
        exchange.getResponseSender().send(ByteBuffer.wrap(
                objectMapper.writeValueAsBytes(
                        Collections.singletonMap("message", "Hello, World!"))));
        */

        //1014821.63 3.32
        exchange.getResponseHeaders().put(Headers.CONTENT_TYPE, "application/json");
        writer.reset();
        MapConverter.serialize(Collections.singletonMap("message", "Hello, World!"), writer);
        exchange.getResponseSender().send(ByteBuffer.wrap(writer.getByteBuffer()));
    }
}
