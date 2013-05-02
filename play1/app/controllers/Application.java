package controllers;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.ExecutionException;

import models.World;
import play.db.jpa.JPAPlugin;
import play.jobs.Job;
import play.libs.F.Promise;
import play.mvc.Controller;

public class Application extends Controller {

	private static final int TEST_DATABASE_ROWS = 10000;

	// FIXME: should this test be consistent - ie set seed or not?
	private static Random random = new Random();

	public static void index() {
		render();
	}

	public static void hello() {
		renderText("hello world");
	}

	public static void json() {
		Map<String, String> result = new HashMap<String, String>();
		result.put("message", "Hello World!");
		renderJSON(result);
	}

	/**
	 * this version is used in the tests. it is the simplest and fastest.
	 * 
	 * @param queries
	 */
	public static void dbSync(int queries) {
		if (queries == 0)
			queries = 1;
		final List<World> worlds = new ArrayList<World>();
		for (int i = 0; i < queries; ++i) {
			Long id = Long.valueOf(random.nextInt(TEST_DATABASE_ROWS) + 1);
			World result = World.findById(id);
			worlds.add(result);
		}
		renderJSON(worlds);
	}

	@play.db.jpa.NoTransaction
	public static void setup() {
		JPAPlugin plugin = play.Play.plugin(JPAPlugin.class);
		plugin.startTx(true);

		// clean out the old
		World.deleteAll();
		System.out.println("DELETED");
		// in with the new
		for (long i = 0; i <= TEST_DATABASE_ROWS; i++) {
			int randomNumber = random.nextInt(TEST_DATABASE_ROWS) + 1;
			new World(i, randomNumber).save();
			if (i % 100 == 0) {

				World.em().flush();
				World.em().clear();
				System.out.println("FLUSHED : " + i + "/" + TEST_DATABASE_ROWS);

			}
		}
		System.out.println("ADDED");
		plugin.closeTx(false);
	}

	/**
	 * note this is method is much slower than the synchronous version
	 */
	public static void dbAsyncEachQuery(int queries)
			throws InterruptedException, ExecutionException {
		if (queries == 0)
			queries = 1;
		final int queryCount = queries;
		List<Promise<World>> promises = new ArrayList<Promise<World>>();
		for (int i = 0; i < queryCount; ++i) {
			final Long id = Long
					.valueOf(random.nextInt(TEST_DATABASE_ROWS) + 1);
			Job<World> job = new Job<World>() {
				public World doJobWithResult() throws Exception {
					World result = World.findById(id);
					return result;
				};
			};
			promises.add(job.now());
		}
		List<World> result = await(Promise.waitAll(promises));
		renderJSON(result);
	}

	/**
	 * note this is method is a bit slower than the synchronous version
	 */
	public static void dbAsyncAllQueries(int queries)
			throws InterruptedException, ExecutionException {
		if (queries == 0)
			queries = 1;
		final int queryCount = queries;
		final List<World> worlds = new ArrayList<World>();
		Job<List<World>> job = new Job<List<World>>() {
			public java.util.List<World> doJobWithResult() throws Exception {
				for (int i = 0; i < queryCount; ++i) {
					Long id = Long
							.valueOf(random.nextInt(TEST_DATABASE_ROWS) + 1);
					World result = World.findById(id);
					worlds.add(result);
				}
				return worlds;
			};
		};
		List<World> result = job.now().get();
		renderJSON(result);
	}

}