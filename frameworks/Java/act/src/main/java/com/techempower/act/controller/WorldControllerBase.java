package com.techempower.act.controller;

import act.app.conf.AutoConfig;
import act.controller.Controller;
import act.db.Dao;
import com.techempower.act.domain.IWorld;
import org.osgl.$;
import org.osgl.mvc.annotation.GetAction;
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
	public final void multipleQueries(String queries) {
		int q = regulateQueries(queries);

		List<MODEL_TYPE> retVal = new ArrayList<>();
		for (int i = 0; i < q; ++i) {
			retVal.add(findOne());
		}
		json(retVal);
	}

	@GetAction("updates")
	public final void updateQueries(String queries) {
		int q = regulateQueries(queries);
		List<MODEL_TYPE> retVal = new ArrayList<>();
		for (int i = 0; i < q; ++i) {
			retVal.add(findAndModifyOne());
		}
		json(retVal);
	}

	protected final MODEL_TYPE findOne() {
		return worldDao.findById(randomWorldNumber());
	}

	protected abstract MODEL_TYPE findAndModifyOne();

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
