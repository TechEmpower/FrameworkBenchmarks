package net.officefloor.benchmark;

import io.vertx.core.Vertx;
import io.vertx.pgclient.PgConnectOptions;
import io.vertx.pgclient.PgConnection;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowIterator;
import io.vertx.sqlclient.SqlConnection;
import io.vertx.sqlclient.Tuple;
import net.officefloor.server.RequestHandler;
import net.officefloor.server.http.HttpResponse;
import net.officefloor.server.http.ServerHttpConnection;
import net.officefloor.server.http.parse.HttpRequestParser;
import net.officefloor.vertx.OfficeFloorVertx;

import java.net.Socket;
import java.util.ArrayList;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedDeque;
import java.util.concurrent.ThreadLocalRandom;

/**
 * R2DBC server.
 *
 * @author Daniel Sagenschneider
 */
public class SqlClientOfficeFloorMain implements DatabaseOperations {

    /**
     * Run application.
     */
    public static void main(String[] args) throws Exception {
        RawWoof.run(args, (socketCount, server, port, database, username,
                           password) -> new SqlClientOfficeFloorMain(socketCount, server, port, database, username, password));
    }

    /**
     * {@link ThreadLocal} {@link PgConnection} instances.
     */
    private final ThreadLocal<PgConnection> threadLocalConnection;

    /**
     * Instantiate.
     *
     * @param socketCount Number of server {@link Socket} instances.
     * @param server      Name of database server.
     * @param port        Port of database.
     * @param database    Name of database within server.
     * @param username    Username.
     * @param password    Password.
     */
    public SqlClientOfficeFloorMain(int socketCount, String server, int port, String database, String username,
                                    String password) {
    	
    	// Obtain the vertx
    	Vertx vertx = OfficeFloorVertx.getVertx();

        // Create connection
        PgConnectOptions connectOptions = new PgConnectOptions().setHost(server).setPort(port).setDatabase(database)
                .setUser(username).setPassword(password);

        // Create thread local connection
        this.threadLocalConnection = new ThreadLocal<PgConnection>() {
            @Override
            protected PgConnection initialValue() {
                try {
                    return OfficeFloorVertx.block(PgConnection.connect(vertx, connectOptions));
                } catch (Exception ex) {
                    throw new IllegalStateException("Failed to setup connection", ex);
                }
            }
        };
    }

    /*
     * ===================== DatabaseOperations ======================
     */

    @Override
    public void threadSetup(RequestHandler<HttpRequestParser> requestHandler) {
        // Nothing thread specific to set up
    }

    @Override
    public void db(HttpResponse response, ServerHttpConnection connection, DatabaseOperationsContext context) {
        this.threadLocalConnection.get().preparedQuery("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID=$1")
                .execute(Tuple.of(ThreadLocalRandom.current().nextInt(1, 10001)),
                        result -> {
                            if (result.failed()) {
                                context.sendError(connection, result.cause());
                            } else {
                                RowIterator<Row> rows = result.result().iterator();
                                if (!rows.hasNext()) {
                                    context.sendError(connection, 404);
                                } else {
                                    Row row = rows.next();
                                    World world = new World(row.getInteger(0), row.getInteger(1));
                                    context.dbSend(response, connection, world);
                                }
                            }
                        });
    }

    @Override
    public void queries(int queryCount, HttpResponse response, ServerHttpConnection connection,
                        DatabaseOperationsContext context) {
        Queue<World> worlds = new ConcurrentLinkedDeque<>();
        SqlConnection sqlConnection = this.threadLocalConnection.get();
        for (int i = 0; i < queryCount; i++) {
            sqlConnection.preparedQuery("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID=$1")
                    .execute(Tuple.of(ThreadLocalRandom.current().nextInt(1, 10001)),
                            result -> {
                                if (result.failed()) {
                                    context.sendError(connection, result.cause());
                                } else {
                                    RowIterator<Row> rows = result.result().iterator();
                                    if (!rows.hasNext()) {
                                        context.sendError(connection, 404);
                                    } else {
                                        Row row = rows.next();
                                        World world = new World(row.getInteger(0), row.getInteger(1));
                                        worlds.add(world);

                                        if (worlds.size() == queryCount) {
                                            context.queriesSend(response, connection, new ArrayList<>(worlds));
                                        }
                                    }
                                }
                            });
        }
    }

    @Override
    public void fortunes(HttpResponse response, ServerHttpConnection connection, DatabaseOperationsContext context) {
        this.threadLocalConnection.get().preparedQuery("SELECT ID, MESSAGE FROM FORTUNE")
                .execute(result -> {
                    if (result.failed()) {
                        context.sendError(connection, result.cause());
                    } else {
                        List<Fortune> fortunes = new ArrayList<>(16);
                        RowIterator<Row> rows = result.result().iterator();
                        while (rows.hasNext()) {
                            Row row = rows.next();
                            fortunes.add(new Fortune(row.getInteger(0), row.getString(1)));
                        }
                        context.fortunesSend(response, connection, fortunes);
                    }
                });
    }

    @Override
    public void update(int queryCount, HttpResponse response, ServerHttpConnection connection,
                       DatabaseOperationsContext context) {
        Queue<World> worlds = new ConcurrentLinkedDeque<>();
        SqlConnection sqlConnection = this.threadLocalConnection.get();
        for (int i = 0; i < queryCount; i++) {
            sqlConnection.preparedQuery("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID=$1")
                    .execute(Tuple.of(ThreadLocalRandom.current().nextInt(1, 10001)),
                            result -> {
                                if (result.failed()) {
                                    context.sendError(connection, result.cause());
                                } else {
                                    RowIterator<Row> rows = result.result().iterator();
                                    if (!rows.hasNext()) {
                                        context.sendError(connection, 404);
                                    } else {
                                        Row row = rows.next();
                                        World world = new World(row.getInteger(0), ThreadLocalRandom.current().nextInt(1, 10001));
                                        worlds.add(world);

                                        if (worlds.size() == queryCount) {

                                            // All worlds obtained, so run update
                                            List<Tuple> batch = new ArrayList<>(queryCount);
                                            for (World update : worlds) {
                                                batch.add(Tuple.of(update.randomNumber, update.id));
                                            }
                                            sqlConnection.preparedQuery("UPDATE world SET randomnumber=$1 WHERE id=$2").executeBatch(batch, ar -> {
                                                if (result.failed()) {
                                                    context.sendError(connection, result.cause());
                                                } else {

                                                    // Updated, so send response
                                                    context.queriesSend(response, connection, new ArrayList<>(worlds));
                                                }
                                            });
                                        }
                                    }
                                }
                            });
        }
    }

}