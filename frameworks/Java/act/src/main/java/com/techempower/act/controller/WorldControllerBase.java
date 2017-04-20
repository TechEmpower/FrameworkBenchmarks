package com.techempower.act.controller;

import act.app.conf.AutoConfig;
import act.controller.Controller;
import act.db.Dao;
import com.techempower.act.domain.IWorld;
import io.ebean.annotation.Transactional;
import org.osgl.$;
import org.osgl.mvc.annotation.GetAction;
import org.osgl.mvc.result.Result;
import org.osgl.util.Const;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

@AutoConfig
public abstract class WorldControllerBase<MODEL_TYPE extends IWorld,
		QUERY_TYPE extends Dao.Query<MODEL_TYPE, QUERY_TYPE>,
		DAO_TYPE extends Dao<Integer, MODEL_TYPE, QUERY_TYPE>> extends Controller.Util {

	/**
     * This constant will get populated with the value set in
	 * `app.world.max_row` configuration item
	 */
	public static final Const<Integer> WORLD_MAX_ROW = $.constant();

	protected DAO_TYPE worldDao;

	public WorldControllerBase(DAO_TYPE worldDao) {
		this.worldDao = $.notNull(worldDao);
	}

	@GetAction("db")
	public final void singleQuery() {
		json(findOne());
	}

	@GetAction("queries")
    @Transactional(readOnly = true)
	public final Result multipleQueries(String queries) {
		int q = regulateQueries(queries);

		IWorld[] worlds = new IWorld[q];
		for (int i = 0; i < q; ++i) {
			worlds[i] = findOne();
		}
		return json(worlds);
	}

	@GetAction("updates")
	public final void updateQueries(String queries) {
		int q = regulateQueries(queries);
		List<MODEL_TYPE> retVal = doUpdate(q);
		json(retVal);
	}

	@Transactional
	protected List<MODEL_TYPE> doUpdate(int q) {
		List<MODEL_TYPE> retVal = new ArrayList<>(q);
		for (int i = 0; i < q; ++i) {
			retVal.add(findAndModifyOne());
		}
		worldDao.save(retVal);
		return retVal;
	}

	protected final MODEL_TYPE findOne() {
		return worldDao.findById(randomWorldNumber());
	}

	protected final MODEL_TYPE findAndModifyOne() {
		MODEL_TYPE world = findOne();
		notFoundIfNull(world);
		world.setRandomNumber(randomWorldNumber());
		return world;
	}

	public static int regulateQueries(String param) {
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

	public static int randomWorldNumber() {
		return ThreadLocalRandom.current().nextInt(WORLD_MAX_ROW.get()) + 1;
	}

}
