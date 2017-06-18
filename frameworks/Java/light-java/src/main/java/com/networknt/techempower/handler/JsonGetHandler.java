package com.networknt.techempower.handler;

import com.dslplatform.json.*;
import io.undertow.server.HttpHandler;
import io.undertow.server.HttpServerExchange;
import io.undertow.util.Headers;

import java.nio.ByteBuffer;

public class JsonGetHandler implements HttpHandler {
    private DslJson<Object> dsl = new DslJson<>();
    private JsonWriter writer = dsl.newWriter(512);

    @Override
    public void handleRequest(HttpServerExchange exchange) throws Exception {
        exchange.getResponseHeaders().put(Headers.CONTENT_TYPE, "application/json");
        writer.reset();
        dsl.serialize(writer, new Message());
        exchange.getResponseSender().send(ByteBuffer.wrap(writer.toByteArray()));
    }

    static class Message implements JsonObject {
        public String message = "Hello, World!";
        @Override
        public void serialize(JsonWriter writer, boolean minimal) {
            writer.writeAscii("{\"message\":");
            StringConverter.serialize(message, writer);
            writer.writeByte(com.dslplatform.json.JsonWriter.OBJECT_END);
        }
    }
}
