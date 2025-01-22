/*
 * Copyright (c) 2022 Oracle and/or its affiliates.
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

package io.helidon.benchmark.nima;

import java.nio.charset.StandardCharsets;
import java.util.logging.Logger;

import io.helidon.benchmark.nima.models.DbRepository;
import io.helidon.benchmark.nima.models.HikariJdbcRepository;
import io.helidon.benchmark.nima.models.PgClientRepository;
import io.helidon.benchmark.nima.services.DbService;
import io.helidon.benchmark.nima.services.FortuneHandler;
import io.helidon.http.Header;
import io.helidon.http.HeaderNames;
import io.helidon.http.HeaderValues;
import io.helidon.config.Config;
import io.helidon.config.ConfigException;
import io.helidon.logging.common.LogConfig;
import io.helidon.webserver.WebServer;
import io.helidon.webserver.http.Handler;
import io.helidon.webserver.http.HttpRules;
import io.helidon.webserver.http.ServerRequest;
import io.helidon.webserver.http.ServerResponse;

import static io.helidon.benchmark.nima.JsonSerializer.serialize;

/**
 * Main class of the benchmark.
 * Opens server on localhost:8080 and exposes {@code /plaintext} and {@code /json} endpoints adhering to the
 * rules of TechEmpower benchmarking.
 */
public final class Main {
    private static final Logger LOGGER = Logger.getLogger(Main.class.getName());

    public static final Header CONTENT_TYPE_HTML =
            HeaderValues.createCached(HeaderNames.CONTENT_TYPE, "text/html; charset=UTF-8");
    public static final Header SERVER = HeaderValues.createCached(HeaderNames.SERVER, "Nima");

    private Main() {
    }

    /**
     * Start the server.
     *
     * @param args ignored
     */
    public static void main(String[] args) {
        // logging and config
        LogConfig.configureRuntime();

        WebServer.builder()
                .config(Config.create().get("server"))
                .routing(Main::routing)
                .build()
                .start();
    }

    // exposed for tests
    @SuppressWarnings("unchecked")
    static void routing(HttpRules rules) {
        Config config = Config.create();

        DbRepository repository;
        String name = config.get("db-repository").asString().orElse("pgclient");
        LOGGER.info("Using '" + name + "' as DB repository");
        if (name.equalsIgnoreCase("hikari")) {
            repository = new HikariJdbcRepository(config);
        } else if (name.equalsIgnoreCase("pgclient")) {
            repository = new PgClientRepository(config);
        } else {
            throw new ConfigException("Allowed values for 'db-repository' are 'hikari' and 'pgclient'");
        }

        rules.get("/plaintext", new PlaintextHandler())
                .get("/json", new JsonHandler())
                .get("/fortunes", new FortuneHandler(repository))
                .register("/", new DbService(repository));
    }

    static class PlaintextHandler implements Handler {
        static final Header CONTENT_TYPE = HeaderValues.createCached(HeaderNames.CONTENT_TYPE,
                                                                     "text/plain; charset=UTF-8");
        static final Header CONTENT_LENGTH = HeaderValues.createCached(HeaderNames.CONTENT_LENGTH, "13");
        private static final byte[] RESPONSE_BYTES = "Hello, World!".getBytes(StandardCharsets.UTF_8);

        @Override
        public void handle(ServerRequest req, ServerResponse res) {
            res.header(CONTENT_LENGTH);
            res.header(CONTENT_TYPE);
            res.header(Main.SERVER);
            res.send(RESPONSE_BYTES);
        }
    }

    static class JsonHandler implements Handler {
        private static final String MESSAGE = "Hello, World!";
        private static final int JSON_LENGTH = serialize(new Message(MESSAGE)).length;
        static final Header CONTENT_LENGTH = HeaderValues.createCached(HeaderNames.CONTENT_LENGTH,
                                                                       String.valueOf(JSON_LENGTH));

        @Override
        public void handle(ServerRequest req, ServerResponse res) {
            res.header(CONTENT_LENGTH);
            res.header(HeaderValues.CONTENT_TYPE_JSON);
            res.header(Main.SERVER);
            res.send(serialize(new Message(MESSAGE)));
        }
    }

    /**
     * Message to be serialized as JSON.
     */
    public static final class Message {

        private final String message;

        /**
         * Construct a new message.
         *
         * @param message message string
         */
        public Message(String message) {
            super();
            this.message = message;
        }

        /**
         * Get message string.
         *
         * @return message string
         */
        public String getMessage() {
            return message;
        }
    }
}
