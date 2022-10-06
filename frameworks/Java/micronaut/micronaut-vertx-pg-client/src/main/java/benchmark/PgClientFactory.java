/*
 * Copyright 2017-2020 original authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package benchmark;

import io.micronaut.context.annotation.Bean;
import io.micronaut.context.annotation.Factory;
import io.micronaut.context.annotation.Property;
import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.core.impl.VertxBuilder;
import io.vertx.pgclient.PgConnectOptions;
import io.vertx.pgclient.PgPool;
import io.vertx.sqlclient.PoolOptions;
import jakarta.inject.Singleton;

/**
 * The Factory for creating Vertx PG client.
 */
@Factory
public class PgClientFactory {

    @Singleton
    @Bean(preDestroy = "close")
    public PgPool client(@Property(name = "datasources.default.url") String url,
                         @Property(name = "datasources.default.username") String user,
                         @Property(name = "datasources.default.password") String password,
                         @Property(name = "datasources.default.maximum-pool-size") int maxPoolSize) {

        VertxOptions vertxOptions = new VertxOptions()
                .setPreferNativeTransport(true);

        PgConnectOptions connectOptions = PgConnectOptions.fromUri(url.substring(5))
                .setUser(user)
                .setPassword(password)
                .setCachePreparedStatements(true)
                .setTcpNoDelay(true)
                .setTcpQuickAck(true)
                .setPipeliningLimit(1024);
        PoolOptions poolOptions = new PoolOptions();
        poolOptions.setMaxSize(maxPoolSize);

        Vertx vertx = new VertxBuilder(vertxOptions).init().vertx();
        return PgPool.pool(vertx, connectOptions, poolOptions);
    }
}
