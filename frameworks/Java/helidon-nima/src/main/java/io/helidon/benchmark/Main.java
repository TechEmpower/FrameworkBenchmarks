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

package io.helidon.benchmark;

import io.helidon.benchmark.models.Repository;
import io.helidon.benchmark.services.DbService;
import io.helidon.benchmark.services.FortuneService;
import io.helidon.benchmark.services.JsonService;
import io.helidon.benchmark.services.PlainTextService;
import io.helidon.config.Config;
import io.helidon.nima.webserver.WebServer;
import io.helidon.nima.webserver.http.HttpRouting;

import java.io.IOException;
import java.util.function.Consumer;
import java.util.logging.LogManager;

public final class Main {

    private static Consumer<HttpRouting.Builder> createRouting(Config config) {
        Repository repository = new Repository(config.get("db"));
        return (routing) -> routing
                .any((req, res) -> {
                    res.header("Server", "Nima");
                    res.next();
                })
                .register("/json", new JsonService())
                .register("/plaintext", new PlainTextService())
                .register("/", new DbService(repository))
                .register("/fortunes", new FortuneService(repository))
                .build();
    }

    public static void main(final String[] args) throws IOException {
        // load logging configuration
        LogManager.getLogManager().readConfiguration(
                Main.class.getResourceAsStream("/logging.properties"));

        // By default this will pick up application.yaml from the classpath
        Config config = Config.create();

        WebServer server = WebServer.builder()
                .host(config.get("server.host").asString().orElse("0.0.0.0"))
                .port(config.get("server.port").asInt().orElse(8080))
                .routing(createRouting(config))
                .start();

        // Start the server and print some info.
        System.out.println("WEB server is up! http://0.0.0.0:" + server.port());
    }
}
