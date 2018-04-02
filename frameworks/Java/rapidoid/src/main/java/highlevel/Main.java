package highlevel;

import common.Helper;
import common.Message;
import org.rapidoid.commons.Err;
import org.rapidoid.config.Conf;
import org.rapidoid.env.Env;
import org.rapidoid.http.MediaType;
import org.rapidoid.jdbc.JDBC;
import org.rapidoid.jdbc.JdbcClient;
import org.rapidoid.setup.App;
import org.rapidoid.setup.On;

public class Main {

	public static void main(String[] args) {
		App.run(args);

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
		JdbcClient jdbc = JDBC.api();

		if (Env.hasProfile("mysql")) {
			jdbc.url("jdbc:mysql://tfb-database:3306/hello_world?" + Helper.MYSQL_CONFIG);

		} else if (Env.hasProfile("postgres")) {
			jdbc.url("jdbc:postgresql://tfb-database:5432/hello_world?" + Helper.POSTGRES_CONFIG);

		} else {
			throw Err.notExpected();
		}

		On.get("/fortunes").managed(false).html(new FortunesHandler(jdbc));
	}

}
