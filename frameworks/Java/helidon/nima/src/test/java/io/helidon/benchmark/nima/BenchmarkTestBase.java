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

import io.helidon.common.http.Headers;
import io.helidon.common.http.Http;
import io.helidon.common.http.Http.HeaderValues;
import io.helidon.nima.testing.junit5.webserver.SetUpRoute;
import io.helidon.nima.webclient.http1.Http1Client;
import io.helidon.nima.webclient.http1.Http1ClientResponse;
import io.helidon.nima.webserver.http.HttpRules;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.RepeatedTest;

import static io.helidon.common.testing.http.junit5.HttpHeaderMatcher.hasHeader;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;

/**
 * This is a test to make sure the endpoints work as expected.
 * This is not a benchmark!
 */
abstract class BenchmarkTestBase {
    private final Http1Client client;

    protected BenchmarkTestBase(Http1Client client) {
        this.client = client;
    }

    @SetUpRoute
    static void routing(HttpRules rules) {
        Main.routing(rules);
    }

    @RepeatedTest(10)
    void testPlaintext() {
        try (Http1ClientResponse response = client.get("/plaintext").request()) {
            Headers headers = response.headers();

            assertThat(response.status(), is(Http.Status.OK_200));
            Assertions.assertAll(
                    () -> assertThat(headers, hasHeader(Main.PlaintextHandler.CONTENT_TYPE)),
                    () -> assertThat(headers, hasHeader(Main.PlaintextHandler.CONTENT_LENGTH)),
                    () -> assertThat(headers, hasHeader(Main.PlaintextHandler.SERVER)),
                    () -> assertThat(headers, hasHeader(HeaderValues.CONNECTION_KEEP_ALIVE)),
                    () -> assertThat(headers, hasHeader(Http.Header.DATE)),
                    () -> assertThat(response.as(String.class), is("Hello, World!"))
            );
        }
    }

    @RepeatedTest(10)
    void testJson() {
        try (Http1ClientResponse response = client.get("/json").request()) {
            Headers headers = response.headers();

            assertThat(response.status(), is(Http.Status.OK_200));
            Assertions.assertAll(
                    () -> assertThat(headers, hasHeader(HeaderValues.CONTENT_TYPE_JSON)),
                    () -> assertThat(headers, hasHeader(Main.JsonHandler.CONTENT_LENGTH)),
                    () -> assertThat(headers, hasHeader(Main.JsonHandler.SERVER)),
                    () -> assertThat(headers, hasHeader(HeaderValues.CONNECTION_KEEP_ALIVE)),
                    () -> assertThat(headers, hasHeader(Http.Header.DATE)),
                    () -> assertThat(response.as(String.class), is("{\"message\":\"Hello, World!\"}"))
            );
        }
    }
}
