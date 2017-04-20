package com.techempower.act.controller;

import act.app.conf.AutoConfig;
import act.controller.Controller;
import com.techempower.act.domain.Fortune;
import com.techempower.act.domain.World;
import org.osgl.$;
import org.osgl.mvc.annotation.GetAction;
import org.osgl.mvc.result.Result;
import org.osgl.util.Const;

import javax.inject.Inject;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

@AutoConfig
public class AppController extends Controller.Util {

	/**
     * This constant will get populated with the value set in
	 * `app.world.max_row` configuration item
	 */
	public static final Const<Integer> WORLD_MAX_ROW = $.constant();

	@Inject
	private World.Dao worldDao;

	@Inject
	private Fortune.Dao fortuneDao;


	@GetAction("db")
	public final void singleQuery() {
		json(findOne());
	}

	@GetAction("queries")
	public final Result multipleQueries(String queries) {
		int q = regulateQueries(queries);

		World[] worlds = new World[q];
		for (int i = 0; i < q; ++i) {
			worlds[i] = findOne();
		}
		return json(worlds);
	}

	@GetAction("updates")
	public final void updateQueries(String queries) {
		int q = regulateQueries(queries);
		List<World> retVal = doUpdate(q);
		json(retVal);
	}

	@GetAction("fortunes")
	public void fortunes(Fortune.Dao fortuneDao) {
		List<Fortune> fortunes = fortuneDao.all();
		fortunes.add(new Fortune(0, "Additional fortune added at request time."));
		Collections.sort(fortunes);
		template("fortunes.mustache", fortunes);
	}

	protected List<World> doUpdate(int q) {
		List<World> retVal = new ArrayList<>(q);
		for (int i = 0; i < q; ++i) {
			retVal.add(findAndModifyOne());
		}
		return retVal;
	}

	private World findOne() {
		return worldDao.single(randomWorldNumber());
	}

	private World findAndModifyOne() {
		World world = findOne();
		notFoundIfNull(world);
		world.setRandomNumber(randomWorldNumber());
		worldDao.updateById(world);
		return world;
	}

	private int regulateQueries(String param) {
		if (null == param) {
			return 1;
		}
		try {
			int val = Integer.parseInt(param);
			if (val < 1) {
				return 1;
			}
			return val > 500 ? 500 : val;
		} catch (NumberFormatException e) {
			return 1;
		}
	}

	protected final int randomWorldNumber() {
		return ThreadLocalRandom.current().nextInt(WORLD_MAX_ROW.get()) + 1;
	}

}
