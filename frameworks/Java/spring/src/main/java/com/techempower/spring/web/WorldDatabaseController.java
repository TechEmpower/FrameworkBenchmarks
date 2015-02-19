package com.techempower.spring.web;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.ThreadLocalRandom;

import com.techempower.spring.Common;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.techempower.spring.domain.World;
import com.techempower.spring.repository.WorldRepository;

@RestController
final class WorldDatabaseController {

	private static final int DB_ROWS = 10000;

	@Autowired
	private WorldRepository worldRepository;

	@RequestMapping(value = "/db", produces = "application/json")
	World singleQuery() {
		final Random random = ThreadLocalRandom.current();
		return this.worldRepository.findOne(random.nextInt(DB_ROWS) + 1);
	}

	@RequestMapping(value = "/queries", produces = "application/json")
	@Transactional(readOnly = true)
	List<World> multipleQueries(@RequestParam(value="queries", required=false, defaultValue="1") String rawQueryCount) {
		Integer queryCount = boundQueryCount(rawQueryCount);

		List<Future<World>> wfs = new ArrayList<>(queryCount);
		// it gets better with Java 8, promise!
		for (int i = 0; i < queryCount; i++) {
			wfs.add(
				Common.EXECUTOR.submit(
					new Callable<World>() {
						@Override
						public World call() throws Exception {
							return worldRepository.findOne(
								ThreadLocalRandom.current().nextInt(DB_ROWS) + 1);
						}
					}));
		}

		return waitFor(wfs);
	}

	@RequestMapping(value = "/updates", produces = "application/json")
	List<World> updateQueries(@RequestParam(value="queries", required=false, defaultValue="1") String rawQueryCount) {
		Integer queryCount = boundQueryCount(rawQueryCount);

		List<Future<World>> wfs = new ArrayList<>(queryCount);

		for (int i = 0; i < queryCount; i++) {
			wfs.add(Common.EXECUTOR.submit(
				new Callable<World>() {
					@Override
					@Transactional(propagation = Propagation.REQUIRES_NEW)
					public World call() throws Exception {
						Random random = ThreadLocalRandom.current();
						World world = worldRepository.findOne(random.nextInt(DB_ROWS) + 1);
						world.setRandomNumber(random.nextInt(DB_ROWS) + 1);
						worldRepository.save(world);
						return world;
					}
				}));
		}

		return waitFor(wfs);
	}

	private List<World> waitFor(List<Future<World>> wfs) {
		List<World> worlds = new ArrayList<>(wfs.size());
		for (Future<World> wf: wfs) {
			try {
				worlds.add(wf.get());
			} catch (InterruptedException | ExecutionException e) {
				throw new RuntimeException(e);
			}
		}
		return worlds;
	}

	private Integer boundQueryCount(final String rawString) {
		Integer raw;
		try {
			raw = Integer.parseInt(rawString);
		} catch (NumberFormatException e) {
			raw = null;
		}
		if (raw == null || raw < 1) {
			return 1;
		} else if (raw > 500) {
			return 500;
		}

		return raw;
	}
}
