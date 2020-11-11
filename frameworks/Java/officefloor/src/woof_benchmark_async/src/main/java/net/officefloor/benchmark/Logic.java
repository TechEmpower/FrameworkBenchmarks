package net.officefloor.benchmark;

import java.io.IOException;
import java.io.Writer;
import java.sql.SQLException;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

import org.apache.commons.text.StringEscapeUtils;

import com.github.mustachejava.DefaultMustacheFactory;
import com.github.mustachejava.Mustache;
import com.github.mustachejava.MustacheFactory;

import lombok.AllArgsConstructor;
import lombok.Data;
import net.officefloor.frame.api.function.AsynchronousFlow;
import net.officefloor.r2dbc.R2dbcSource;
import net.officefloor.server.http.HttpHeaderValue;
import net.officefloor.server.http.HttpResponse;
import net.officefloor.server.http.ServerHttpConnection;
import net.officefloor.web.HttpQueryParameter;
import net.officefloor.web.ObjectResponse;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

/**
 * Logic.
 */
public class Logic {

	static {
		// Increase the buffer size
		System.setProperty("reactor.bufferSize.small", String.valueOf(10000));
	}

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

	public void db(AsynchronousFlow async, R2dbcSource source, ObjectResponse<World> response) throws SQLException {
		source.getConnection().flatMap(
				connection -> Mono.from(connection.createStatement("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID = $1")
						.bind(0, ThreadLocalRandom.current().nextInt(1, 10001)).execute()))
				.flatMap(result -> Mono.from(result.map((row, metadata) -> {
					Integer id = row.get(0, Integer.class);
					Integer number = row.get(1, Integer.class);
					return new World(id, number);
				}))).subscribe(world -> async.complete(() -> {
					response.send(world);
				}), error -> async.complete(() -> {
					throw error;
				}));
	}

	// ========== QUERIES ==================

	public void queries(@HttpQueryParameter("queries") String queries, AsynchronousFlow async, R2dbcSource source,
			ObjectResponse<List<World>> response) {
		int queryCount = getQueryCount(queries);
		source.getConnection().flatMap(connection -> {
			return Flux.range(1, queryCount)
					.flatMap(index -> connection.createStatement("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID = $1")
							.bind(0, ThreadLocalRandom.current().nextInt(1, 10001)).execute())
					.flatMap(result -> Flux.from(result.map((row, metadata) -> {
						Integer id = row.get(0, Integer.class);
						Integer number = row.get(1, Integer.class);
						return new World(id, number);
					}))).collectList();
		}).subscribe(worlds -> async.complete(() -> {
			response.send(worlds);
		}), error -> async.complete(() -> {
			throw error;
		}));
	}

	// =========== UPDATES ===================

	public void update(@HttpQueryParameter("queries") String queries, AsynchronousFlow async, R2dbcSource source,
			ObjectResponse<List<World>> response) {
		int queryCount = getQueryCount(queries);
		source.getConnection().flatMap(connection -> {
			return Flux.range(1, queryCount)
					.flatMap(index -> connection.createStatement("SELECT ID, RANDOMNUMBER FROM WORLD WHERE ID = $1")
							.bind(0, ThreadLocalRandom.current().nextInt(1, 10001)).execute())
					.flatMap(result -> Flux.from(result.map((row, metadata) -> {
						Integer id = row.get(0, Integer.class);
						Integer number = row.get(1, Integer.class);
						return new World(id, number);
					}))).flatMap(world -> {
						world.randomNumber = ThreadLocalRandom.current().nextInt(1, 10001);
						return Flux
								.from(connection.createStatement("UPDATE WORLD SET RANDOMNUMBER = $1 WHERE ID = $2")
										.bind(0, world.randomNumber).bind(1, world.id).execute())
								.flatMap(result -> Flux.from(result.getRowsUpdated()).map(updated -> world));
					}).collectList();
		}).subscribe(worlds -> async.complete(() -> {
			response.send(worlds);
		}), error -> async.complete(() -> {
			throw error;
		}));
	}

	// =========== FORTUNES ==================

	private static final HttpHeaderValue TEXT_HTML = new HttpHeaderValue("text/html;charset=utf-8");

	@Data
	@AllArgsConstructor
	public class Fortune {

		private int id;

		private String message;
	}

	public void fortunes(AsynchronousFlow async, R2dbcSource source, ServerHttpConnection httpConnection)
			throws IOException, SQLException {
		source.getConnection().flatMap(connection -> {
			return Flux.from(connection.createStatement("SELECT ID, MESSAGE FROM FORTUNE").execute())
					.flatMap(result -> Flux.from(result.map((row, metadata) -> {
						Integer id = row.get(0, Integer.class);
						String message = row.get(1, String.class);
						return new Fortune(id, message);
					}))).collectList();
		}).subscribe(fortunes -> async.complete(() -> {
			try {
				// Additional fortunes
				fortunes.add(new Fortune(0, "Additional fortune added at request time."));
				Collections.sort(fortunes, (a, b) -> a.message.compareTo(b.message));

				// Send response
				HttpResponse response = httpConnection.getResponse();
				response.setContentType(TEXT_HTML, null);
				this.fortuneMustache.execute(response.getEntityWriter(), fortunes);
			} catch (IOException ex) {
				ex.printStackTrace();
			}
		}), error -> async.complete(() -> {
			throw error;
		}));
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

}