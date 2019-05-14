package hello;

import hello.domain.World;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.ThreadLocalRandom;

import javax.inject.Inject;
import javax.inject.Singleton;
import javax.persistence.EntityManager;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

import org.hibernate.Session;
import org.hibernate.SessionFactory;
import org.hibernate.Transaction;

@Singleton
@Produces(MediaType.APPLICATION_JSON)
@Path("/db")
public class WorldResource {
	@Inject
	private EntityManager em;

//	@GET
//	public Object db() throws InterruptedException, ExecutionException {
//		Callable<World> callable = () -> {
//			Session session = sessionFactory.openSession();
//			session.setDefaultReadOnly(true);
//			try {
//				return (World) session.byId(World.class).load(randomWorld());
//			} finally {
//				session.close();
//			}
//		};
//		Future<World> futureWorld = Common.EXECUTOR.submit(callable);
//		return futureWorld.get();
//	}
//
//	@GET
//	@Path("/queries")
//	public Object queries(@QueryParam("queries") String queriesParam) throws InterruptedException,
//			ExecutionException {
//		final int queries = getQueries(queriesParam);
//		final World[] worlds = new World[queries];
//
//		Callable<World[]> callable = () -> {
//			Session session = sessionFactory.openSession();
//			session.setDefaultReadOnly(true);
//			try {
//				for (int i = 0; i < queries; i++) {
//					worlds[i] = (World) session.byId(World.class).load(randomWorld());
//				}
//				return worlds;
//			} finally {
//				session.close();
//			}
//		};
//		Future<World[]> futureWorlds = Common.EXECUTOR.submit(callable);
//		return futureWorlds.get();
//	}
//
//	@GET
//	@Path("/updates")
//	public World[] updates(@QueryParam("queries") String queriesParam) throws InterruptedException,
//			ExecutionException {
//		final int queries = getQueries(queriesParam);
//		final World[] worlds = new World[queries];
//
//		Callable<World[]> callable = () -> {
//			Session session = sessionFactory.openSession();
//			session.setDefaultReadOnly(false);
//			Transaction txn = session.beginTransaction();
//
//			try {
//				// using write batching. See the data source properties provided
//				// in the configuration file
//				for (int i = 0; i < queries; i++) {
//					final World world = (World) session.byId(World.class).load(randomWorld());
//					world.randomNumber = randomWorld();
//					session.persist(world);
//					worlds[i] = world;
//				}
//				session.flush();
//				session.clear();
//				txn.commit();
//
//				return worlds;
//			} catch (RuntimeException e) {
//				if (txn != null && txn.isActive())
//					txn.rollback();
//				throw e;
//			} finally {
//				session.close();
//			}
//		};
//		Future<World[]> futureWorlds = Common.EXECUTOR.submit(callable);
//		return futureWorlds.get();
//	}

	private static int getQueries(String proto) {
		int result = 1;
		try {
			if (proto != null && !proto.trim().isEmpty()) {
				result = Integer.parseInt(proto);
			}
		} catch (NumberFormatException ignored) {/* by test contract */
		}

		return Math.min(500, Math.max(1, result));
	}

	private static int randomWorld() {
		return 1 + ThreadLocalRandom.current().nextInt(10000);
	}
}