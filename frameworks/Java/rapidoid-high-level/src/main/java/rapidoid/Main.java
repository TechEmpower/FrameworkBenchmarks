package rapidoid;

import org.rapidoid.config.Conf;
import org.rapidoid.http.Req;
import org.rapidoid.http.ReqHandler;
import org.rapidoid.log.Log;
import org.rapidoid.render.Template;
import org.rapidoid.render.Templates;
import org.rapidoid.setup.App;
import org.rapidoid.setup.On;
import org.rapidoid.sql.JDBC;
import org.rapidoid.sql.JdbcClient;

import java.util.Collections;
import java.util.List;
import java.util.concurrent.Callable;

public class Main {

	// this configuration was copied from the undertow-example project
	private static final String MYSQL_CONFIG = "jdbcCompliantTruncation=false" +
		"&elideSetAutoCommits=true&useLocalSessionState=true&cachePrepStmts=true&cacheCallableStmts=true" +
		"&alwaysSendSetIsolation=false&prepStmtCacheSize=4096&cacheServerConfiguration=true&prepStmtCacheSqlLimit=2048" +
		"&zeroDateTimeBehavior=convertToNull&traceProtocol=false&useUnbufferedInput=false&useReadAheadInput=false" +
		"&maintainTimeStats=false&useServerPrepStmts&cacheRSMetadata=true";

	public static void main(String[] args) {

		App.args(args,
			"production",
			"c3p0.maxPoolSize=150",
			"c3p0.maxIdleTimeExcessConnections=150",
			"c3p0.maxStatementsPerConnection=3",
			"http.serverName=X",
			"http.mandatoryHeaders.connection=false",
			"http.timeout=0");

		String dbHost = Conf.ROOT.entry("dbhost").or("localhost");
		Log.info("Database hostname is: " + dbHost);

		On.port(8080);

		On.get("/plaintext").staticPlain("Hello, world!".getBytes());

		On.get("/json").managed(false).json(new Callable<Object>() {
			@Override
			public Object call() throws Exception {
				return new Message("Hello world!");
			}
		});

		final JdbcClient jdbc = JDBC.newApi()
			.url("jdbc:mysql://" + dbHost + ":3306/hello_world?" + MYSQL_CONFIG)
			.username("benchmarkdbuser")
			.password("benchmarkdbpass")
			.pooled();

		final Template fortunesTmpl = Templates.load("fortunes.html");

		On.get("/fortunes/mysql").html(new ReqHandler() {
			@Override
			public Object execute(Req req) throws Exception {
				return fortunesTmpl.renderToBytes(readFortunes(jdbc));
			}
		});
	}

	public static List<Fortune> readFortunes(JdbcClient jdbc) {
		List<Fortune> fortunes = jdbc.query(Fortune.class, "SELECT * FROM fortune");
		fortunes.add(new Fortune(0, "Additional fortune added at request time."));
		Collections.sort(fortunes);
		return fortunes;
	}

}
