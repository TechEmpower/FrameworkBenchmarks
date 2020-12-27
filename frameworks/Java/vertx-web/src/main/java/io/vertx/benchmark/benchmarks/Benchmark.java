package io.vertx.benchmark.benchmarks;

import io.vertx.ext.web.RoutingContext;

public interface Benchmark {

    public void dbHandler(final RoutingContext ctx);

    public void queriesHandler(final RoutingContext ctx);

    public void fortunesHandler(final RoutingContext ctx);

    public void updateHandler(final RoutingContext ctx);

}
