package net.officefloor.benchmark;

import java.io.IOException;
import java.io.Writer;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

import org.apache.commons.text.StringEscapeUtils;

import com.github.mustachejava.DefaultMustacheFactory;
import com.github.mustachejava.Mustache;
import com.github.mustachejava.MustacheFactory;

import io.vertx.core.CompositeFuture;
import io.vertx.core.Future;
import io.vertx.sqlclient.Pool;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowIterator;
import io.vertx.sqlclient.RowSet;
import io.vertx.sqlclient.Tuple;
import lombok.AllArgsConstructor;
import lombok.Data;
import net.officefloor.frame.api.function.AsynchronousFlow;
import net.officefloor.server.http.HttpHeaderValue;
import net.officefloor.server.http.HttpResponse;
import net.officefloor.server.http.ServerHttpConnection;
import net.officefloor.web.HttpQueryParameter;
import net.officefloor.web.ObjectResponse;

/**
 * Logic.
 */
public class Logic {

	/**
	 * {@link Mustache} for /fortunes.
	 */
	private final Mustache fortuneMustache;

	public Logic() {

		// Load the mustache fortunes template
		MustacheFactory mustacheFactory = new DefaultMustacheFactory() {
			@Override
			public void encode(String value, Writer writer) {
				try {
					StringEscapeUtils.ESCAPE_HTML4.translate(value, writer);
				} catch (IOException ex) {
					ex.printStackTrace();
				}
			}
		};
		this.fortuneMustache = mustacheFactory.compile("fortunes.mustache");
	}

	// =========== PLAINTEXT ===================

	public void plaintext(ServerHttpConnection connection) throws IOException {
		connection.getResponse().getEntityWriter().append("Hello, World!");
	}

	// =========== JSON ===================

	@Data
	public static class Message {
		private final String message;
	}

	public void json(ObjectResponse<Message> response) {
		response.send(new Message("Hello, World!"));
	}

	// ============ DB ====================

	@Data
	@AllArgsConstructor
	public class World {
		private int id;
		private int randomNumber;
	}

	public void db(AsynchronousFlow async, Pool pool, ObjectResponse<World> response) throws SQLException {
		Future<World> future = pool.withConnection((connection) -> {
			return connection.preparedQuery("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID=$1")
					.execute(Tuple.of(ThreadLocalRandom.current().nextInt(1, 10001))).map((rowSet) -> {
						RowIterator<Row> rows = rowSet.iterator();
						if (!rows.hasNext()) {
							return null;
						}
						Row row = rows.next();
						return new World(row.getInteger(0), row.getInteger(1));
					});
		});
		complete(async, future, (world) -> response.send(world));
	}

	// ========== QUERIES ==================

	public void queries(@HttpQueryParameter("queries") String queries, AsynchronousFlow async, Pool pool,
			ObjectResponse<List<World>> response) {
		int queryCount = getQueryCount(queries);
		Future<CompositeFuture> future = pool.withConnection((connection) -> {
			@SuppressWarnings("rawtypes")
			List<Future> futures = new ArrayList<>(queryCount);
			for (int i = 0; i < queryCount; i++) {
				futures.add(connection.preparedQuery("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID=$1")
						.execute(Tuple.of(ThreadLocalRandom.current().nextInt(1, 10001))).map((rowSet) -> {
							RowIterator<Row> rows = rowSet.iterator();
							if (!rows.hasNext()) {
								return null;
							}
							Row row = rows.next();
							return new World(row.getInteger(0), row.getInteger(1));
						}));
			}
			return CompositeFuture.all(futures);
		});
		complete(async, future, (worlds) -> response.send(worlds.list()));
	}

	// =========== UPDATES ===================

	public void update(@HttpQueryParameter("queries") String queries, AsynchronousFlow async, Pool pool,
			ObjectResponse<List<World>> response) {
		int queryCount = getQueryCount(queries);
		Future<List<World>> future = pool.withConnection((connection) -> {
			@SuppressWarnings("rawtypes")
			List<Future> futures = new ArrayList<>(queryCount);

			// Run queries to get the worlds
			for (int i = 0; i < queryCount; i++) {
				futures.add(connection.preparedQuery("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID=$1")
						.execute(Tuple.of(ThreadLocalRandom.current().nextInt(1, 10001))).map((rowSet) -> {
							RowIterator<Row> rows = rowSet.iterator();
							if (!rows.hasNext()) {
								return null;
							}
							Row row = rows.next();

							// Ensure change to random number to trigger update
							int previousRandomNumber = row.getInteger(1);
							int newRandomNumber;
							do {
								newRandomNumber = ThreadLocalRandom.current().nextInt(1, 10001);
							} while (previousRandomNumber == newRandomNumber);

							return new World(row.getInteger(0), newRandomNumber);
						}));
			}
			return CompositeFuture.all(futures).flatMap((compositeFuture) -> {
				List<World> worlds = compositeFuture.list();

				// Sort worlds to avoid deadlocks on updates
				Collections.sort(worlds, (a, b) -> a.id - b.id);

				// All worlds obtained, so run update
				List<Tuple> batch = new ArrayList<>(queryCount);
				for (World update : worlds) {
					batch.add(Tuple.of(update.randomNumber, update.id));
				}
				return connection.preparedQuery("UPDATE world SET randomnumber=$1 WHERE id=$2").executeBatch(batch)
						.map((updates) -> worlds);
			});
		});
		complete(async, future, (worlds) -> response.send(worlds));
	}

	// =========== FORTUNES ==================

	private static final HttpHeaderValue TEXT_HTML = new HttpHeaderValue("text/html;charset=utf-8");

	@Data
	@AllArgsConstructor
	public class Fortune {

		private int id;

		private String message;
	}

	public void fortunes(AsynchronousFlow async, Pool pool, ServerHttpConnection httpConnection) {
		Future<RowSet<Row>> future = pool.withConnection((connection) -> {
			return connection.preparedQuery("SELECT ID, MESSAGE FROM FORTUNE").execute();
		});
		complete(async, future, (rowSet) -> {

			// Obtain the fortunes
			List<Fortune> fortunes = new ArrayList<>(16);
			RowIterator<Row> rows = rowSet.iterator();
			while (rows.hasNext()) {
				Row row = rows.next();
				fortunes.add(new Fortune(row.getInteger(0), row.getString(1)));
			}

			// Additional fortunes
			fortunes.add(new Fortune(0, "Additional fortune added at request time."));
			Collections.sort(fortunes, (a, b) -> a.message.compareTo(b.message));

			// Send response
			HttpResponse response = httpConnection.getResponse();
			response.setContentType(TEXT_HTML, null);
			this.fortuneMustache.execute(response.getEntityWriter(), fortunes);
		});
	}

	// =========== helper ===================

	private static int getQueryCount(String queries) {
		try {
			int count = Integer.parseInt(queries);
			return (count < 1) ? 1 : (count > 500) ? 500 : count;
		} catch (NumberFormatException ex) {
			return 1;
		}
	}

	@FunctionalInterface
	private static interface Completion<T> {
		void complete(T result) throws Exception;
	}

	private static <T> void complete(AsynchronousFlow async, Future<T> future, Completion<T> writeResponse) {
		future.onComplete(result -> {
			if (result.failed()) {
				async.complete(() -> {
					result.cause().printStackTrace();
					throw result.cause();
				});
			} else {
				async.complete(() -> writeResponse.complete(result.result()));
			}
		});
	}

}