package hello.web;

import java.util.Collections;
import java.util.List;

import hello.Utils;
import hello.model.Fortune;
import hello.model.World;
import hello.repository.DbRepository;
import io.jstach.jstachio.JStachio;

import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.function.ServerRequest;
import org.springframework.web.servlet.function.ServerResponse;

@Component
public class DbHandler {

	private final DbRepository dbRepository;

	public DbHandler(DbRepository dbRepository) {
		this.dbRepository = dbRepository;
	}

	ServerResponse db(ServerRequest request) {
		return ServerResponse.ok()
				.header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
				.body(dbRepository.getWorld(Utils.randomWorldNumber()));
	}

	ServerResponse queries(ServerRequest request) {
		int queries = parseQueryCount(request.params().getFirst("queries"));
		World[] worlds = Utils.randomWorldNumbers()
				.mapToObj(dbRepository::getWorld).limit(queries)
				.toArray(World[]::new);
		return ServerResponse.ok()
				.header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
				.body(worlds);
	}

	ServerResponse updates(ServerRequest request) {
		int queries = parseQueryCount(request.params().getFirst("queries"));
		List<World> worlds = Utils.randomWorldNumbers()
				.mapToObj(id -> {
					World world = dbRepository.getWorld(id);
					int randomNumber;
					do {
						randomNumber = Utils.randomWorldNumber();
					} while (randomNumber == world.randomNumber);
					world.randomNumber = randomNumber;
					return world;
				}).limit(queries)
				.toList();
		dbRepository.updateWorlds(worlds);
		return ServerResponse.ok()
				.header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
				.body(worlds);
	}

	ServerResponse fortunes(ServerRequest request) {
		List<Fortune> fortunes = dbRepository.fortunes();
		fortunes.add(new Fortune(0, "Additional fortune added at request time."));
		Collections.sort(fortunes);
		return ServerResponse.ok()
				.header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_HTML_VALUE)
				.body(JStachio.render(new Fortunes(fortunes)));
	}

	private static int parseQueryCount(String textValue) {
		if (textValue == null) {
			return 1;
		}
		int parsedValue;
		try {
			parsedValue = Integer.parseInt(textValue);
		} catch (NumberFormatException e) {
			return 1;
		}
		return Math.min(500, Math.max(1, parsedValue));
	}
}
