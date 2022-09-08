package hello.controller;

import static java.util.Comparator.comparing;

import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.IntStream;

import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import hello.model.Fortune;
import hello.model.World;
import hello.repository.DbRepository;

@RestController
public final class HelloController {

	private DbRepository dbRepository;

	public HelloController(DbRepository dbRepository) {
		this.dbRepository = dbRepository;
	}

	@GetMapping(value = "/plaintext", produces = MediaType.TEXT_PLAIN_VALUE)
	String plaintext() {
		return "Hello, World!";
	}

	@GetMapping("/json")
	Message json() {
		return new Message("Hello, World!");
	}

	@GetMapping("/db")
	World db() {
		return dbRepository.getWorld(randomWorldNumber());
	}

	@GetMapping("/queries")
	World[] queries(@RequestParam(required = false) String queries) {
		return randomWorldNumbers().mapToObj(dbRepository::getWorld).limit(parseQueryCount(queries))
				.toArray(World[]::new);
	}

	@GetMapping("/updates")
	World[] updates(@RequestParam(required = false) String queries) {
		return randomWorldNumbers().mapToObj(dbRepository::getWorld).map(world -> {
			// Ensure that the new random number is not equal to the old one.
			// That would cause the JPA-based implementation to avoid sending the
			// UPDATE query to the database, which would violate the test
			// requirements.

			// Locally the records doesn't exist, maybe in the yours is ok but we need to
			// make this check
			if (world == null) {
				return null;
			}

			int newRandomNumber;
			do {
				newRandomNumber = randomWorldNumber();
			} while (newRandomNumber == world.randomnumber);

			return dbRepository.updateWorld(world, newRandomNumber);
		}).limit(parseQueryCount(queries)).toArray(World[]::new);
	}

	@GetMapping("/fortunes")
	@ModelAttribute("fortunes")
	List<Fortune> fortunes() {
		var fortunes = dbRepository.fortunes();

		fortunes.add(new Fortune(0, "Additional fortune added at request time."));
		fortunes.sort(comparing(fortune -> fortune.message));
		return fortunes;
	}

	private static final int MIN_WORLD_NUMBER = 1;
	private static final int MAX_WORLD_NUMBER_PLUS_ONE = 10_001;

	private static int randomWorldNumber() {
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
