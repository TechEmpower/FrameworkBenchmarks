package hello.web;

import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.IntStream;

import hello.UpdateWorldService;
import hello.model.Fortune;
import hello.model.World;
import hello.repository.DbRepository;

import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.function.RenderingResponse;
import org.springframework.web.servlet.function.ServerRequest;
import org.springframework.web.servlet.function.ServerResponse;

import static java.util.Comparator.comparing;

@Component
public class DbHandler {

	private DbRepository dbRepository;
	private UpdateWorldService updateWorldService;

	public DbHandler(DbRepository dbRepository, UpdateWorldService updateWorldService) {
		this.dbRepository = dbRepository;
		this.updateWorldService = updateWorldService;
	}

	ServerResponse db(ServerRequest request) {
		return ServerResponse.ok()
				.contentType(MediaType.APPLICATION_JSON)
				.body(dbRepository.getWorld(randomWorldNumber()));
	}

	ServerResponse queries(ServerRequest request) {
		String queries = request.params().getFirst("queries");
		World[] worlds = randomWorldNumbers()
				.mapToObj(dbRepository::getWorld).limit(parseQueryCount(queries))
				.toArray(World[]::new);
		return ServerResponse.ok()
				.contentType(MediaType.APPLICATION_JSON)
				.body(worlds);
	}

	ServerResponse updates(ServerRequest request) {
		String queries = request.params().getFirst("queries");
		World[] worlds = randomWorldNumbers()
				.mapToObj(id -> updateWorldService.updateWorld(id))
				.limit(parseQueryCount(queries)).toArray(World[]::new);
		return ServerResponse.ok()
				.contentType(MediaType.APPLICATION_JSON)
				.body(worlds);
	}

	ServerResponse fortunes(ServerRequest request) {
		var fortunes = dbRepository.fortunes();
		fortunes.add(new Fortune(0, "Additional fortune added at request time."));
		fortunes.sort(comparing(fortune -> fortune.message));
		return RenderingResponse
				.create("fortunes")
				.modelAttribute("fortunes", fortunes)
				.header(HttpHeaders.CONTENT_TYPE, MediaType.TEXT_HTML_VALUE)
				.build();
	}

	private static final int MIN_WORLD_NUMBER = 1;
	private static final int MAX_WORLD_NUMBER_PLUS_ONE = 10_001;

	public static int randomWorldNumber() {
		return ThreadLocalRandom.current().nextInt(MIN_WORLD_NUMBER, MAX_WORLD_NUMBER_PLUS_ONE);
	}

	private static IntStream randomWorldNumbers() {
		return ThreadLocalRandom.current().ints(MIN_WORLD_NUMBER, MAX_WORLD_NUMBER_PLUS_ONE)
				// distinct() allows us to avoid using Hibernate's first-level cache in
				// the JPA-based implementation. Using a cache like that would bypass
				// querying the database, which would violate the test requirements.
				.distinct();
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
