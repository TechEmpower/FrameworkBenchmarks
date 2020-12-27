package io.vertx.benchmark;

import io.vertx.benchmark.benchmarks.*;
import io.vertx.core.AbstractVerticle;
import io.vertx.ext.web.Router;

public class App extends AbstractVerticle {

  int port = 8080;

  @Override
  public void start() {
    final Router app = Router.router(vertx);

    final MongoDBBenchmark mongoDBBenchmark = new MongoDBBenchmark(vertx, config());
    final PgClientBenchmark pgClientBenchmark= new PgClientBenchmark(vertx, config());
    final DatabaseSqlBenchmark databaseSqlBenchmark = new DatabaseSqlBenchmark(vertx, config());
    final JsonBenchmark jsonBenchmark = new JsonBenchmark(vertx);
    final PlaintextBenchmark plaintextBenchmark = new PlaintextBenchmark(vertx);

    /*
     * This test exercises the framework fundamentals including keep-alive support, request routing, request header
     * parsing, object instantiation, JSON serialization, response header generation, and request count throughput.
     */
    app.get("/json").handler(jsonBenchmark::json);

    /*
     * This test exercises the framework's object-relational mapper (ORM), random number generator, database driver,
     * and database connection pool.
     */
    app.get("/mongo/db").handler(mongoDBBenchmark::dbHandler);
    app.get("/psql/db").handler(pgClientBenchmark::dbHandler);
    app.get("/dbpsql/db").handler(databaseSqlBenchmark::dbHandler);

    /*
     * This test is a variation of Test #2 and also uses the World table. Multiple rows are fetched to more dramatically
     * punish the database driver and connection pool. At the highest queries-per-request tested (20), this test
     * demonstrates all frameworks' convergence toward zero requests-per-second as database activity increases.
     */
    app.get("/mongo/queries").handler(mongoDBBenchmark::queriesHandler);
    app.get("/psql/queries").handler(pgClientBenchmark::queriesHandler);
    app.get("/dbpsql/queries").handler(databaseSqlBenchmark::queriesHandler);

    /*
     * This test exercises the ORM, database connectivity, dynamic-size collections, sorting, server-side templates,
     * XSS countermeasures, and character encoding.
     */
    app.get("/mongo/fortunes").handler(mongoDBBenchmark::fortunesHandler);
    app.get("/psql/fortunes").handler(pgClientBenchmark::fortunesHandler);
    app.get("/dbpsql/fortunes").handler(databaseSqlBenchmark::fortunesHandler);

    /*
     * This test is a variation of Test #3 that exercises the ORM's persistence of objects and the database driver's
     * performance at running UPDATE statements or similar. The spirit of this test is to exercise a variable number of
     * read-then-write style database operations.
     */
    app.route("/mongo/update").handler(mongoDBBenchmark::updateHandler);
    app.route("/psql/update").handler(pgClientBenchmark::updateHandler);
    app.route("/dbpsql/update").handler(databaseSqlBenchmark::updateHandler);

    /*
     * This test is an exercise of the request-routing fundamentals only, designed to demonstrate the capacity of
     * high-performance platforms in particular. Requests will be sent using HTTP pipelining. The response payload is
     * still small, meaning good performance is still necessary in order to saturate the gigabit Ethernet of the test
     * environment.
     */
    app.get("/plaintext").handler(plaintextBenchmark::plaintext);

    vertx.createHttpServer().requestHandler(app).listen(port, listen -> {
      if (listen.failed()) {
        listen.cause().printStackTrace();
        System.exit(1);
      }else {
        System.out.println("Vert.x run on port:"+port);
      }
    });

  }

}
