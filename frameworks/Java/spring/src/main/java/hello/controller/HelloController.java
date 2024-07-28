package hello.controller;

import static java.util.Comparator.comparing;

import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.IntStream;

import jakarta.servlet.http.HttpServletResponse;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import hello.UpdateWorldService;
import hello.model.Fortune;
import hello.model.World;
import hello.repository.DbRepository;

@RestController
public final class HelloController {

	private DbRepository dbRepository;
	private UpdateWorldService updateWorldService;

	public HelloController(
			DbRepository dbRepository,
			UpdateWorldService updateWorldService) {
		this.dbRepository = dbRepository;
		this.updateWorldService = updateWorldService;
	}

	@GetMapping(value = "/plaintext")
	String plaintext(HttpServletResponse response) {
		response.setContentType(MediaType.TEXT_PLAIN_VALUE);
		return "Hello, World!";
	}

	@GetMapping("/json")
	Message json(HttpServletResponse response) {
		response.setContentType(MediaType.APPLICATION_JSON_VALUE);
		return new Message("Hello, World!");
	}

	@GetMapping("/db")
	World db(HttpServletResponse response) {
		response.setContentType(MediaType.APPLICATION_JSON_VALUE);
		return dbRepository.getWorld(randomWorldNumber());
	}

	@GetMapping("/queries")
	World[] queries(HttpServletResponse response, @RequestParam(required = false) String queries) {
		response.setContentType(MediaType.APPLICATION_JSON_VALUE);
		return randomWorldNumbers().mapToObj(dbRepository::getWorld).limit(parseQueryCount(queries))
				.toArray(World[]::new);
	}

	@GetMapping("/updates")
	World[] updates(HttpServletResponse response, @RequestParam(required = false) String queries) {
		response.setContentType(MediaType.APPLICATION_JSON_VALUE);
		return randomWorldNumbers()
				.mapToObj(id -> updateWorldService.updateWorld(id))
				.limit(parseQueryCount(queries)).toArray(World[]::new);
	}

	@GetMapping("/fortunes")
	@ModelAttribute("fortunes")
	List<Fortune> fortunes(HttpServletResponse response) {
		response.setContentType(MediaType.TEXT_HTML_VALUE);
		var fortunes = dbRepository.fortunes();

		fortunes.add(new Fortune(0, "Additional fortune added at request time."));
		fortunes.sort(comparing(fortune -> fortune.message));
		return fortunes;
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

	static class Message {
		private final String message;

		public Message(String message) {
			this.message = message;
		}

		public String getMessage() {
			return message;
		}
	}
}
