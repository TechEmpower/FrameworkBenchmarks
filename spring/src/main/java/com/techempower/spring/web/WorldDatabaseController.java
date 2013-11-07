package com.techempower.spring.web;

import com.techempower.spring.domain.World;
import com.techempower.spring.service.WorldRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

import java.util.Arrays;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

@Controller
public class WorldDatabaseController {

	private static final int DB_ROWS = 10000;
	@Autowired
	private WorldRepository worldRepository;

	@RequestMapping(value = "/db", produces = "application/json")
	@ResponseBody
	public World singleQuery() {
		final Random random = ThreadLocalRandom.current();
		return worldRepository.findOne(random.nextInt(DB_ROWS) + 1);
	}

	@RequestMapping(value = "/queries", produces = "application/json")
	@ResponseBody
	public World[] multipleQueries(Integer queries) {
		if (queries == null || queries < 1) {
			queries = 1;
		}
		else if (queries > 500) {
			queries = 500;
		}

		final World[] worlds = new World[queries];
		final Random random = ThreadLocalRandom.current();

		for (int i = 0; i < queries; i++) {
			worlds[i] = worldRepository.findOne(random.nextInt(DB_ROWS) + 1);
		}

		return worlds;
	}

	@RequestMapping(value = "/updates", produces = "application/json")
	@ResponseBody
	public World[] updateQueries(Integer queries) {
		if (queries == null || queries < 1) {
			queries = 1;
		}
		else if (queries > 500) {
			queries = 500;
		}

		final World[] worlds = multipleQueries(queries);
		final Random random = ThreadLocalRandom.current();

		for (int i = 0; i < queries; i++) {
			World world = worlds[i];
			world.randomNumber = random.nextInt(DB_ROWS) + 1;
		}
		worldRepository.save(Arrays.asList(worlds));

		return worlds;
	}
}
