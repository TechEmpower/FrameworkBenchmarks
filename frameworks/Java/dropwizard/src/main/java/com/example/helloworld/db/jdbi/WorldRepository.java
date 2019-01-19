package com.example.helloworld.db.jdbi;

import org.skife.jdbi.v2.DBI;

import com.example.helloworld.db.WorldDAO;
import com.example.helloworld.db.model.World;
import com.example.helloworld.resources.Helper;

public class WorldRepository implements WorldDAO {
	private DBI jdbi;

	public WorldRepository(DBI jdbi) {
		super();
		this.jdbi = jdbi;
	}

	@Override
	public World findById(int id) {
		try (WorldJDBIImpl dao = jdbi.open(WorldJDBIImpl.class)) {
			return dao.findById(id);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public World[] findById(int[] ids) {
		final World[] worlds = new World[ids.length];
		try (WorldJDBIImpl dao = jdbi.open(WorldJDBIImpl.class)) {
			return dao.inTransaction((conn, status) -> {
				for (int i = 0; i < ids.length; i++) {
					worlds[i] = dao.findById(ids[i]);
				}
				return worlds;
			});
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	@Override
	public World findAndModify(int id, int newRandomNumber) {
		throw new RuntimeException("Don't call this");
	}

	@Override
	public World[] updatesQueries(int totalQueries) {
		final World[] updates = new World[totalQueries];
		try (WorldJDBIImpl dao = jdbi.open(WorldJDBIImpl.class)) {
				for (int i = 0; i < totalQueries; i++) {
					updates[i] = dao.inTransaction((conn, status) -> {
						final World world = dao.findById(Helper.randomWorld());
						world.setRandomNumber(Helper.randomWorld());
						dao.update(world);
						return world;
					});
				}
				return updates;
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

}