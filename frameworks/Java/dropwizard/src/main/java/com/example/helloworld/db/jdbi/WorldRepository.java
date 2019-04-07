package com.example.helloworld.db.jdbi;

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
			final World[] updates = new World[totalQueries];

			for (int i = 0; i < totalQueries; i++) {
				final World world = dao.findById(Helper.randomWorld());
				world.setRandomNumber(Helper.randomWorld());
				updates[i] = world;
			}

			dao.update(updates);

			return updates;
		});
	}

}