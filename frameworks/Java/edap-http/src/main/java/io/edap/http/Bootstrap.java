package io.edap.http;

import io.edap.http.model.Message;
import io.edap.Edap;
import io.edap.http.server.HttpServer;
import io.edap.http.server.HttpServerBuilder;
import io.edap.json.Eson;
import io.edap.json.JsonCodecRegister;
import io.edap.json.JsonEncoder;
import io.edap.json.JsonWriter;

import java.io.IOException;

import static io.edap.http.header.ContentTypeHeader.JSON;
import static io.edap.http.header.ContentTypeHeader.PLAIN;

public class Bootstrap {

    static final byte[] PLAIN_TEXT_CONTENT = "Hello, World!".getBytes();

    static JsonEncoder<Message> ENCODER = JsonCodecRegister.instance().getEncoder(Message.class);

    public static void main(String[] args) throws IOException {
        HttpHandleOption option = new HttpHandleOption();
        HttpHandleOption optionPiplining = new HttpHandleOption();
        String lazyParseHeaderStr = System.getProperty("lazyParseHeader", "false");
        if (Boolean.parseBoolean(lazyParseHeaderStr)) {
            option.setLazyParseHeader(true);
            optionPiplining.setLazyParseHeader(true);
        }

        final Message msg = new Message("Hello, World!");

        //optionPiplining.setEnablePipelining(true);

        HttpServer httpServer = new HttpServerBuilder()
                .listen(8080)
                .req("/plaintext", (req, resp) ->
						resp.contentType(PLAIN).write(PLAIN_TEXT_CONTENT), optionPiplining)
                .get("/json", (req, resp) -> {
                    JsonWriter writer = Eson.THREAD_WRITER.get();
                    try {
                        writer.reset();
                        ENCODER.encode(writer, msg);
                        resp.contentType(JSON).write(writer);
                    } catch (Exception e) {
                        throw new RuntimeException(e);
                    }
                }, option)
                .build();
        Edap edap = new Edap();
        edap.addServer(httpServer);
        edap.run();
    }
}
