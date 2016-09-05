package highlevel;

import common.Helper;
import common.Message;
import org.rapidoid.config.Conf;
import org.rapidoid.http.MediaType;
import org.rapidoid.log.Log;
import org.rapidoid.setup.App;
import org.rapidoid.setup.On;
import org.rapidoid.sql.JDBC;
import org.rapidoid.sql.JdbcClient;

public class Main {

	public static void main(String[] args) {

		App.args(args,
			"production",
			"c3p0.maxPoolSize=256",
			"c3p0.maxIdleTimeExcessConnections=256",
			"c3p0.maxStatementsPerConnection=3",
			"http.serverName=X",
			"http.mandatoryHeaders.connection=false",
			"http.timeout=0");

		String dbHost = Conf.ROOT.entry("dbhost").or("localhost");
		Log.info("Database hostname is: " + dbHost);

		On.port(8080);

		On.get("/plaintext").managed(false).contentType(MediaType.TEXT_PLAIN).serve("Hello, world!");

		On.get("/json").managed(false).json(() -> new Message("Hello, world!"));

		JdbcClient mysqlJdbc = JDBC.newApi()
			.url("jdbc:mysql://" + dbHost + ":3306/hello_world?" + Helper.MYSQL_CONFIG)
			.username("benchmarkdbuser")
			.password("benchmarkdbpass")
			.pooled();

		On.get("/fortunes/mysql").html(new FortunesHandler(mysqlJdbc));
	}

}
