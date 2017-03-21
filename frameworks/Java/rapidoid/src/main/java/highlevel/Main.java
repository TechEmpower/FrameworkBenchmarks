package highlevel;

import common.Helper;
import common.Message;
import org.rapidoid.commons.Env;
import org.rapidoid.config.Conf;
import org.rapidoid.http.MediaType;
import org.rapidoid.log.Log;
import org.rapidoid.setup.App;
import org.rapidoid.setup.On;
import org.rapidoid.sql.JDBC;
import org.rapidoid.sql.JdbcClient;

public class Main {

	public static void main(String[] args) {

		App.args(args);

		Conf.C3P0.set("maxPoolSize", 256);
		Conf.C3P0.set("maxIdleTimeExcessConnections", 256);
		Conf.C3P0.set("maxStatementsPerConnection", 3);

		Conf.HTTP.set("maxPipeline", 128);
		Conf.HTTP.set("timeout", 0);
		Conf.HTTP.sub("mandatoryHeaders").set("connection", false);

		On.port(8080);

		if (Env.hasAnyProfile("mysql", "postgres")) {
			setupDbHandlers();
		} else {
			setupSimpleHandlers();
		}
	}

	private static void setupSimpleHandlers() {
		On.get("/plaintext").managed(false).contentType(MediaType.TEXT_PLAIN).serve("Hello, world!");
		On.get("/json").managed(false).json(() -> new Message("Hello, world!"));
	}

	private static void setupDbHandlers() {
		String dbHost = Conf.ROOT.entry("dbhost").or("localhost");
		Log.info("Database hostname is: " + dbHost);

		String dbUrl;

		if (Env.hasProfile("mysql")) {
			dbUrl = "jdbc:mysql://" + dbHost + ":3306/hello_world?" + Helper.MYSQL_CONFIG;
		} else {
			dbUrl = "jdbc:postgresql://" + dbHost + ":5432/hello_world?" + Helper.POSTGRES_CONFIG;
		}

		JdbcClient mysqlJdbc = JDBC.newApi()
			.url(dbUrl)
			.username("benchmarkdbuser")
			.password("benchmarkdbpass")
			.pooled();

		On.get("/fortunes").html(new FortunesHandler(mysqlJdbc));
	}

}
