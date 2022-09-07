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

import io.helidon.nima.testing.junit5.webserver.ServerTest;
import io.helidon.nima.webclient.http1.Http1Client;

@ServerTest
class BenchmarkIT extends BenchmarkTestBase {
    protected BenchmarkIT(Http1Client client) {
        super(client);
    }
}
