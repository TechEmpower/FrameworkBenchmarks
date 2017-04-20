package com.techempower.act.beetlsql;

import act.app.conf.AutoConfig;
import act.controller.Controller;
import org.osgl.$;
import org.osgl.mvc.annotation.GetAction;
import org.osgl.mvc.result.Result;
import org.osgl.util.Const;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static com.techempower.act.controller.WorldControllerBase.randomWorldNumber;
import static com.techempower.act.controller.WorldControllerBase.regulateQueries;

/**
 * Testing for Act on BeetlSQL
 */
@Controller("beetlsql")
@AutoConfig
@SuppressWarnings("unused")
public class BeetlSqlController extends Controller.Util {

    /**
     * This constant will get populated with the value set in
     * `app.world.max_row` configuration item
     */
    public static final Const<Integer> WORLD_MAX_ROW = $.constant();

    @GetAction("db")
    public final void singleQuery(World.Mapper worldDao) {
        json(findOne(worldDao));
    }

    @GetAction("queries")
    public final Result multipleQueries(String queries, World.Mapper worldDao) {
        int q = regulateQueries(queries);

        World[] worlds = new World[q];
        for (int i = 0; i < q; ++i) {
            worlds[i] = findOne(worldDao);
        }
        return json(worlds);
    }

    @GetAction("updates")
    public final void updateQueries(String queries, World.Mapper worldDao) {
        int q = regulateQueries(queries);
        List<World> retVal = doUpdate(q, worldDao);
        json(retVal);
    }

    @GetAction("fortunes")
    public void fortunes(Fortune.Mapper fortuneDao) {
        List<Fortune> fortunes = fortuneDao.all();
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes);
        template("fortunes.mustache", fortunes);
    }

    protected List<World> doUpdate(int q, World.Mapper worldDao) {
        List<World> retVal = new ArrayList<>(q);
        for (int i = 0; i < q; ++i) {
            retVal.add(findAndModifyOne(worldDao));
        }
        return retVal;
    }

    private World findOne(World.Mapper worldDao) {
        return worldDao.single(randomWorldNumber());
    }

    private World findAndModifyOne(World.Mapper worldDao) {
        World world = findOne(worldDao);
        notFoundIfNull(world);
        world.setRandomNumber(randomWorldNumber());
        worldDao.updateById(world);
        return world;
    }


}
