package com.example.helloworld.db.hibernate;

import org.hibernate.SessionFactory;
import org.hibernate.Transaction;

import com.example.helloworld.db.WorldDAO;
import com.example.helloworld.db.model.World;
import com.example.helloworld.resources.Helper;

import io.dropwizard.hibernate.AbstractDAO;

public class WorldHibernateImpl extends AbstractDAO<World> implements WorldDAO {
	public WorldHibernateImpl(SessionFactory factory) {
		super(factory);
	}

	@Override
	public World findById(int id) {
		return get(id);
	}

	@Override
	public World[] findById(int[] ids) {
		World[] worlds = new World[ids.length];
		for (int i = 0; i < ids.length; i++) {
			worlds[i] = get(ids[i]);
		}
		return worlds;
	}

	@Override
	public World findAndModify(int id, int newRandomNumber) {
		final World world = get(id);
		world.setRandomNumber(newRandomNumber);
		return persist(world);
	}

	private World modify(World world, int newRandomNumber) {
		world.setRandomNumber(newRandomNumber);
		return persist(world);
	}

	/**
	 * Using manual transaction handling and JDBC batch updates
	 */
	@Override
	public World[] updatesQueries(int totalQueries) {
		final World[] worlds = new World[totalQueries];
		Transaction txn = null;

		try {
			txn = currentSession().beginTransaction();

			// using write batching. See the data source properties provided in the
			// configuration .yml file
			int[] ids = Helper.getRandomInts(totalQueries);
			int i = 0;
			for (int id : ids) {
				int newNumber;
				World world = findById(id);
				do {
					newNumber = Helper.randomWorld();
				} while (world.getRandomNumber() == newNumber);
				worlds[i++] = modify(world, newNumber);
			}
			currentSession().flush();
			currentSession().clear();
			txn.commit();
		} catch (RuntimeException e) {
			if (txn != null && txn.isActive())
				txn.rollback();
			throw e;
		}
		// The cleaning of the session should happen in the dropwizard provided aspect
		// code

		return worlds;
	}

}