package com.example.helloworld.db.jdbi;

import java.util.Arrays;
import java.util.Comparator;

import org.jdbi.v3.core.Jdbi;

import com.example.helloworld.db.WorldDAO;
import com.example.helloworld.db.model.World;
import com.example.helloworld.resources.Helper;

public class WorldRepository implements WorldDAO {
	private final Jdbi jdbi;

	public WorldRepository(Jdbi jdbi) {
		this.jdbi = jdbi;
	}

	@Override
	public World findById(int id) {
		return jdbi.withExtension(WorldJDBIImpl.class, dao -> dao.findById(id));
	}

	@Override
	public World findAndModify(int id, int newRandomNumber) {
		throw new RuntimeException("Don't call this");
	}

	@Override
	public World[] updatesQueries(int totalQueries) {
		return jdbi.withExtension(WorldJDBIImpl.class, dao -> {
			final World updates[] = new World[totalQueries];

			for (int i = 0; i < totalQueries; i++) {
				final World world = dao.findById(Helper.randomWorld());
				world.setRandomNumber(Helper.randomWorld());
				updates[i] = world;
			}

			// Reason for sorting : https://github.com/TechEmpower/FrameworkBenchmarks/pull/2684
//			updates.sort(Comparator.comparingInt(World::getId));
			Arrays.sort(updates, Comparator.comparingInt(World::getId));

//			final World[] updatesArray = updates.toArray(new World[totalQueries]);

			dao.update(updates);

			return updates;
		});
	}

}