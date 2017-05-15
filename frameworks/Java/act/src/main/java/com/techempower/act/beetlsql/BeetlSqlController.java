package com.techempower.act.beetlsql;

import act.controller.Controller;
import org.osgl.mvc.annotation.GetAction;
import org.osgl.mvc.result.Result;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static com.techempower.act.controller.WorldControllerBase.randomWorldNumber;
import static com.techempower.act.controller.WorldControllerBase.regulateQueries;

/**
 * Testing for Act on BeetlSQL
 */
@Controller("beetlsql")
@SuppressWarnings("unused")
public class BeetlSqlController extends Controller.Util {

    @GetAction("db")
    public final void singleQuery(World.Mapper worldMapper) {
        json(findOne(worldMapper));
    }

    @GetAction("queries")
    public final Result multipleQueries(String queries, World.Mapper worldMapper) {
        int q = regulateQueries(queries);

        World[] worlds = new World[q];
        for (int i = 0; i < q; ++i) {
            worlds[i] = findOne(worldMapper);
        }
        return json(worlds);
    }

    @GetAction("updates")
    public final void updateQueries(String queries, World.Mapper worldMapper) {
        int q = regulateQueries(queries);
        List<World> retVal = doUpdate(q, worldMapper);
        json(retVal);
    }

    @GetAction("fortunes")
    public void fortunes(Fortune.Mapper fortuneMapper) {
        List<Fortune> fortunes = fortuneMapper.all();
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes);
        template("fortunes.mustache", fortunes);
    }

    protected List<World> doUpdate(int q, World.Mapper worldMapper) {
        List<World> retVal = new ArrayList<>(q);
        for (int i = 0; i < q; ++i) {
            retVal.add(findAndModifyOne(worldMapper));
        }
        return retVal;
    }

    private World findOne(World.Mapper worldMapper) {
        return worldMapper.single(randomWorldNumber());
    }

    private World findAndModifyOne(World.Mapper worldMapper) {
        World world = findOne(worldMapper);
        notFoundIfNull(world);
        world.setRandomNumber(randomWorldNumber());
        worldMapper.updateById(world);
        return world;
    }


}
