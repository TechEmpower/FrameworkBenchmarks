package com.techempower.benchmark.pippo;

import com.techempower.benchmark.pippo.dao.Dao;
import com.techempower.benchmark.pippo.dao.MongoDao;
import com.techempower.benchmark.pippo.dao.SqlDao;
import com.techempower.benchmark.pippo.handler.Test1JsonHandler;
import com.techempower.benchmark.pippo.handler.Test2SingleQueryHandler;
import com.techempower.benchmark.pippo.handler.Test3MultiQueryHandler;
import com.techempower.benchmark.pippo.handler.Test4FortuneHandler;
import com.techempower.benchmark.pippo.handler.Test5UpdateHandler;
import com.techempower.benchmark.pippo.handler.Test6PlainTextHandler;
import ro.pippo.core.Application;
import ro.pippo.pebble.PebbleTemplateEngine;

public class BenchmarkApplication extends Application {

	@Override
	protected void onInit() {

		setTemplateEngine(new PebbleTemplateEngine());

		postgresql = new SqlDao("postgresql");
		mysql = new SqlDao("mysql");
		mongo = new MongoDao();

		GET("/json", new Test1JsonHandler());

		GET("/postgres/db", new Test2SingleQueryHandler(postgresql));
		GET("/mysql/db", new Test2SingleQueryHandler(mysql));
		GET("/mongo/db", new Test2SingleQueryHandler(mongo));

		GET("/postgres/queries", new Test3MultiQueryHandler(postgresql));
		GET("/mysql/queries", new Test3MultiQueryHandler(mysql));
		GET("/mongo/queries", new Test3MultiQueryHandler(mongo));

		GET("/postgres/fortunes", new Test4FortuneHandler(postgresql));
		GET("/mysql/fortunes", new Test4FortuneHandler(mysql));
		GET("/mongo/fortunes", new Test4FortuneHandler(mongo));

		GET("/postgres/updates", new Test5UpdateHandler(postgresql));
		GET("/mysql/updates", new Test5UpdateHandler(mysql));
		GET("/mongo/updates", new Test5UpdateHandler(mongo));

		GET("/plaintext", new Test6PlainTextHandler());

	}

	@Override
	protected void onDestroy() {

		try {
			postgresql.close();
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		try {
			mysql.close();
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		try {
			mongo.close();
		} catch (Exception e) {
			throw new RuntimeException(e);
		}

		super.onDestroy();

	}

	private Dao postgresql;
	private Dao mysql;
	private Dao mongo;

}
