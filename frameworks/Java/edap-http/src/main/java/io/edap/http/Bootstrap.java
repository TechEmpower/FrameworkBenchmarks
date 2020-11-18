package io.edap.http;

import io.edap.http.model.Message;
import io.edap.x.Edap;
import io.edap.x.http.HttpServer;
import io.edap.x.http.HttpServerBuilder;

import java.io.IOException;

import static io.edap.x.http.header.ContentType.JSON;
import static io.edap.x.http.header.ContentType.PLAIN;

public class Bootstrap {

    public static void main(String[] args) throws IOException {

        HttpServer httpServer = new HttpServerBuilder()
                .listen(8080)
                .req("/plaintext", (req, resp) -> resp.contentType(PLAIN).write("Hello, World!"))
                .get("/json", (req, resp) -> resp.contentType(JSON).write(new Message("Hello, World!")))
                .build();
        Edap edap = new Edap();
        edap.addServer(httpServer);
        edap.run();
    }
}
