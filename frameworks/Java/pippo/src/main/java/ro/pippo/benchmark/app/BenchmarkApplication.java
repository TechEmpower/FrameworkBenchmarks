package ro.pippo.benchmark.app;

import ro.pippo.benchmark.dao.Dao;
import ro.pippo.benchmark.dao.MongoDao;
import ro.pippo.benchmark.dao.SqlDao;
import ro.pippo.benchmark.handlers.Test1Handler;
import ro.pippo.benchmark.handlers.Test2Handler;
import ro.pippo.benchmark.handlers.Test3Handler;
import ro.pippo.benchmark.handlers.Test4Handler;
import ro.pippo.benchmark.handlers.Test5Handler;
import ro.pippo.benchmark.handlers.Test6Handler;
import ro.pippo.core.Application;
import ro.pippo.pebble.PebbleTemplateEngine;

public class BenchmarkApplication extends Application {

  @Override
  protected void onInit() {
    setTemplateEngine(new PebbleTemplateEngine());

    Dao mysql = new SqlDao(getPippoSettings(), "db.mysql");
    Dao psql = new SqlDao(getPippoSettings(), "db.psql");
    Dao mongo = new MongoDao(getPippoSettings());

    GET("/json", new Test1Handler());

    GET("/mysql/db", new Test2Handler(mysql));
    GET("/psql/db", new Test2Handler(psql));
    GET("/mongo/db", new Test2Handler(mongo));

    GET("/mysql/queries", new Test3Handler(mysql));
    GET("/psql/queries", new Test3Handler(psql));
    GET("/mongo/queries", new Test3Handler(mongo));

    GET("/mysql/fortunes", new Test4Handler(mysql));
    GET("/psql/fortunes", new Test4Handler(psql));
    GET("/mongo/fortunes", new Test4Handler(mongo));

    GET("/mysql/updates", new Test5Handler(mysql));
    GET("/psql/updates", new Test5Handler(psql));
    GET("/mongo/updates", new Test5Handler(mongo));

    GET("/plaintext", new Test6Handler());
  }
}