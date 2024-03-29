
/*
 * Copyright (c) 2018, 2021 Oracle and/or its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.helidon.benchmark.reactive;

import java.io.IOException;
import java.util.logging.LogManager;

import io.helidon.benchmark.reactive.models.DbRepository;
import io.helidon.benchmark.reactive.models.JdbcRepository;
import io.helidon.benchmark.reactive.services.DbService;
import io.helidon.benchmark.reactive.services.FortuneService;
import io.helidon.benchmark.reactive.services.JsonService;
import io.helidon.benchmark.reactive.services.PlainTextService;
import io.helidon.config.Config;
import io.helidon.media.jsonp.JsonpSupport;
import io.helidon.webserver.Routing;
import io.helidon.webserver.WebServer;

/**
 * Simple Hello World rest application.
 */
public final class Main {

    private static final String SERVER_HEADER = "Server";
    private static final String SERVER_NAME = "Helidon";

    /**
     * Cannot be instantiated.
     */
    private Main() {
    }

    private static DbRepository getRepository(Config config) {
        return new JdbcRepository(config);
    }

    /**
     * Creates new {@link Routing}.
     *
     * @return the new instance
     */
    private static Routing createRouting(Config config) {
        DbRepository repository = getRepository(config);

        return Routing.builder()
                .any((req, res) -> {
                    res.headers().add(SERVER_HEADER, SERVER_NAME);
                    req.next();
                })
                .register(new JsonService())
                .register(new PlainTextService())
                .register(new DbService(repository))
                .register(new FortuneService(repository))
                .build();
    }

    /**
     * Application main entry point.
     *
     * @param args command line arguments.
     * @throws IOException if there are problems reading logging properties
     */
    public static void main(final String[] args) throws IOException {
        startServer();
    }

    /**
     * Start the server.
     *
     * @return the created {@link WebServer} instance
     * @throws IOException if there are problems reading logging properties
     */
    private static WebServer startServer() throws IOException {

        // load logging configuration
        LogManager.getLogManager().readConfiguration(
                Main.class.getResourceAsStream("/logging.properties"));

        // By default this will pick up application.yaml from the classpath
        Config config = Config.create();

        // Build server with JSONP support
        WebServer server = WebServer.builder(createRouting(config))
                .config(config.get("server"))
                .addMediaSupport(JsonpSupport.create())
                .build();

        // Start the server and print some info.
        server.start().thenAccept(ws -> {
            System.out.println("WEB server is up! http://localhost:" + ws.port());
        });

        // Server threads are not demon. NO need to block. Just react.
        server.whenShutdown().thenRun(()
                -> System.out.println("WEB server is DOWN. Good bye!"));

        return server;
    }
}
