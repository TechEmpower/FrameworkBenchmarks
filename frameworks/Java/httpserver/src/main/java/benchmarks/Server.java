package benchmarks;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.text.ParseException;
import java.util.*;
import java.util.concurrent.Executors;
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import javax.sql.DataSource;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.module.afterburner.AfterburnerModule;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;
import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import httl.Engine;
import httl.Template;

public class Server {

    private static final String HELLO_TEXT = "Hello, World!";
    private static final byte[] HELLO_BYTES = HELLO_TEXT.getBytes();
    private static final int HELLO_LENGTH = HELLO_BYTES.length;
    private static final String SERVER_NAME = "httpserver";
    private static final ObjectMapper MAPPER = new ObjectMapper();

    static {
        MAPPER.registerModule(new AfterburnerModule());
    }

    private static List<Fortune> queryFortunes(DataSource ds) throws SQLException {
        List<Fortune> fortunes = new ArrayList<>();
        try (Connection conn = ds.getConnection();
             PreparedStatement statement = conn.prepareStatement("SELECT id, message FROM fortune");
             ResultSet resultSet = statement.executeQuery()) {
            while (resultSet.next())
                fortunes.add(new Fortune(resultSet.getInt(1), resultSet.getString(2)));
        }
        return fortunes;
    }

    private static DataSource createPostgresDataSource() throws ClassNotFoundException {
        Class.forName("org.postgresql.Driver");
        HikariConfig config = new HikariConfig();
        config.setJdbcUrl("jdbc:postgresql://tfb-database:5432/hello_world");
        config.setUsername("benchmarkdbuser");
        config.setPassword("benchmarkdbpass");
        config.setMaximumPoolSize(512);
        return new HikariDataSource(config);
    }

    private static Template loadTemplate(String filename) throws IOException, ParseException {
        Properties props = new Properties();
        props.put("import.packages", "java.util," + Fortune.class.getPackage().getName());
        props.put("input.encoding", "UTF-8");
        props.put("output.encoding", "UTF-8");
        props.put("precompiled", "false");
        Engine engine = Engine.getEngine(props);
        return engine.getTemplate(filename);
    }

    private static HttpHandler createPlaintextHandler() {
        return t -> {
            t.getResponseHeaders().add("Content-Type", "text/plain");
            t.getResponseHeaders().add("Server", SERVER_NAME);
            t.sendResponseHeaders(200, HELLO_LENGTH);
            t.getResponseBody().write(HELLO_BYTES);
            t.getResponseBody().flush();
            t.getResponseBody().close();
        };
    }

    private static HttpHandler createJSONHandler() {
        return t -> {
            // serialize message to JSON
            Message msg = new Message(HELLO_TEXT);
            byte[] bytes;
            try {
                bytes = MAPPER.writeValueAsBytes(msg);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
            // send response
            t.getResponseHeaders().add("Content-Type", "application/json");
            t.getResponseHeaders().add("Server", SERVER_NAME);
            t.sendResponseHeaders(200, bytes.length);
            t.getResponseBody().write(bytes);
            t.getResponseBody().flush();
            t.getResponseBody().close();
        };
    }

    private static HttpHandler createFortunesHandler(DataSource ds) throws IOException, ParseException {
        Template template = loadTemplate("/fortunes.template.httl");
        return t -> {
            try {
                // query db
                List<Fortune> fortunes = queryFortunes(ds);
                fortunes.add(new Fortune(0, "Additional fortune added at request time."));
                Collections.sort(fortunes);
                // render template
                Map<String, Object> context = new HashMap<>(1);
                context.put("fortunes", fortunes);
                ByteArrayOutputStream out = new ByteArrayOutputStream();
                template.render(context, out);
                byte[] bytes = out.toByteArray();
                // send response
                t.getResponseHeaders().add("Content-Type", "text/html; charset=utf-8");
                t.getResponseHeaders().add("Server", SERVER_NAME);
                t.sendResponseHeaders(200, bytes.length);
                t.getResponseBody().write(bytes);
                t.getResponseBody().flush();
                t.getResponseBody().close();
            } catch (SQLException | ParseException e) {
                throw new IOException(e);
            }
        };
    }

    public static void main(String[] args) throws Exception {
        // parse arguments
        String settings = args.length > 0 ? args[0] : "";
        int port = args.length > 1 ? Integer.parseInt(args[1]) : 8080;
        if (settings.contains("debug"))
            System.setProperty("org.slf4j.simpleLogger.defaultLogLevel", "DEBUG");
        // create server
        HttpServer server = HttpServer.create(new InetSocketAddress(port), 1024 * 8);
        server.setExecutor(Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors()));
        // add context handlers
        server.createContext("/plaintext", createPlaintextHandler());
        server.createContext("/json", createJSONHandler());
        if (settings.contains("postgres")) {
            DataSource ds = createPostgresDataSource();
            server.createContext("/fortunes", createFortunesHandler(ds));
        }
        // start server
        server.start();
    }
}
