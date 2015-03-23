package sabina.benchmark;

import static java.lang.Integer.parseInt;
import static sabina.Sabina.*;
import static sabina.content.JsonContent.toJson;

import com.mchange.v2.c3p0.ComboPooledDataSource;
import sabina.Exchange;
import sabina.Request;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Date;
import java.util.Properties;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

import javax.sql.DataSource;

/**
 * When it is implemented, add this to benchmark_config
 * "fortune_url": "/fortune",
 * "update_url": "/update",
 */
final class Application {
    private static final Properties CONFIG = loadConfig ();
    private static final DataSource DS = createSessionFactory ();
    private static final String QUERY = "select * from world where id = ?";

    private static final int DB_ROWS = 10000;
    private static final String MESSAGE = "Hello, World!";
    private static final String CONTENT_TYPE_TEXT = "text/plain";

    private static Properties loadConfig () {
        try {
            Properties config = new Properties ();
            config.load (Class.class.getResourceAsStream ("/server.properties"));
            return config;
        }
        catch (Exception ex) {
            throw new RuntimeException (ex);
        }
    }

    private static DataSource createSessionFactory () {
        try {
            ComboPooledDataSource cpds = new ComboPooledDataSource ();
            cpds.setJdbcUrl (CONFIG.getProperty ("mysql.uri"));
            cpds.setMinPoolSize (32);
            cpds.setMaxPoolSize (256);
            cpds.setCheckoutTimeout (1800);
            cpds.setMaxStatements (50);
            return cpds;
        }
        catch (Exception ex) {
            throw new RuntimeException (ex);
        }
    }

    private static int getQueries (final Request request) {
        try {
            String param = request.queryParams ("queries");
            if (param == null)
                return 1;

            int queries = parseInt (param);
            if (queries < 1)
                return 1;
            if (queries > 500)
                return 500;

            return queries;
        }
        catch (NumberFormatException ex) {
            return 1;
        }
    }

    private static Object getJson (Exchange it) {
        it.response.type ("application/json");
        return toJson (new Message ());
    }

    private static Object getDb (Exchange it) {
        final int queries = getQueries (it.request);
        final World[] worlds = new World[queries];

        try (final Connection con = DS.getConnection ()) {
            final Random random = ThreadLocalRandom.current ();
            PreparedStatement stmt = con.prepareStatement (QUERY);

            for (int i = 0; i < queries; i++) {
                stmt.setInt (1, random.nextInt (DB_ROWS) + 1);
                ResultSet rs = stmt.executeQuery ();
                while (rs.next ()) {
                    worlds[i] = new World ();
                    worlds[i].id = rs.getInt (1);
                    worlds[i].randomNumber = rs.getInt (2);
                }
            }
        }
        catch (SQLException e) {
            e.printStackTrace ();
        }

        it.response.type ("application/json");
        return toJson (it.request.queryParams ("queries") == null? worlds[0] : worlds);
    }

    private static Object getFortune (Exchange aExchange) {
        throw new UnsupportedOperationException ();
    }

    private static Object getUpdate (Exchange aExchange) {
        throw new UnsupportedOperationException ();
    }

    private static Object getPlaintext (Exchange it) {
        it.response.type (CONTENT_TYPE_TEXT);
        return MESSAGE;
    }

    private static void addCommonHeaders (Exchange it) {
        it.header ("Server", "Undertow/1.1.2");
        it.response.raw ().addDateHeader ("Date", new Date ().getTime ());
    }

    public static void main (String[] args) {
        get ("/json", Application::getJson);
        get ("/db", Application::getDb);
        get ("/fortune", Application::getFortune);
        get ("/update", Application::getUpdate);
        get ("/plaintext", Application::getPlaintext);
        after (Application::addCommonHeaders);

        setIpAddress (CONFIG.getProperty ("web.host"));
        start (parseInt (CONFIG.getProperty ("web.port")));
    }
}
