package com.example.helloworld.db.hibernate;

import io.dropwizard.hibernate.AbstractDAO;

import org.hibernate.SessionFactory;
import org.hibernate.Transaction;

import com.example.helloworld.db.WorldDAO;
import com.example.helloworld.db.model.World;
import com.example.helloworld.resources.Helper;

public class WorldHibernateImpl extends AbstractDAO<World> implements WorldDAO {
	public WorldHibernateImpl(SessionFactory factory) {
		super(factory);
	}

	public World findById(int id) {
		return get(id);
	}

	public World findAndModify(int id, int newRandomNumber) {
		final World world = get(id);
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

			// using write batching. See the data source properties provided in the configuration .yml file
			for (int i = 0; i < totalQueries; i++) {
				worlds[i] = findAndModify(Helper.randomWorld(), Helper.randomWorld());
			}
			currentSession().flush();
			currentSession().clear();
			txn.commit();
		} catch (RuntimeException e) {
			if (txn != null && txn.isActive())
				txn.rollback();
			throw e;
		}
		// The cleaning of the session should happen in the dropwizard provided aspect code

		return worlds;
	}

}