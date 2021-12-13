package com.techempower.act.controller;

/*-
 * #%L
 * TEB ActFramework Project
 * %%
 * Copyright (C) 2016 - 2017 ActFramework
 * %%
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * #L%
 */

import static act.controller.Controller.Util.notFoundIfNull;

import act.app.conf.AutoConfig;
import act.db.Dao;
import act.db.sql.tx.Transactional;
import act.sys.Env;
import act.util.Global;
import act.util.JsonView;
import com.techempower.act.AppEntry;
import com.techempower.act.model.World;
import org.osgl.mvc.annotation.GetAction;
import org.osgl.mvc.annotation.SessionFree;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.concurrent.ThreadLocalRandom;
import javax.inject.Inject;

@AutoConfig
@Env.RequireProfile(value = AppEntry.PROFILE_JSON_PLAINTEXT, except = true)
@JsonView
@SessionFree
public class WorldController {

    private static boolean BATCH_SAVE;

    /**
     * This constant will get populated with the value set in
     * `app.world.max_row` configuration item
     */
    private static int WORLD_MAX_ROW = 10000;

    @Global
    @Inject
    private Dao<Integer, World, ?> dao;


    @GetAction("db")
    public World findOne() {
        return dao.findById(randomWorldNumber());
    }

    @GetAction("queries")
    public final World[] multipleQueries(String queries) {
        int q = regulateQueries(queries);

        World[] worlds = ThreadLocalRandom
            .current()
            .ints(1, WORLD_MAX_ROW + 1)
            .distinct()
            .limit(q)
            .mapToObj(id -> dao.findById(id))
            .toArray(World[]::new);
        return worlds;
    }

    @GetAction("updates")
    public final List<World> updateQueries(String queries) {
        int q = regulateQueries(queries);
        return doUpdate(q);
    }

    @Transactional
    private List<World> doUpdate(int q) {
        List<World> retVal = ThreadLocalRandom
            .current()
            .ints(1, WORLD_MAX_ROW + 1)
            .distinct()
            .limit(q)
            .sorted()
            .mapToObj(id -> findAndModifyOne(id))
            .collect(Collectors.toCollection(ArrayList::new));
        if (BATCH_SAVE) {
            batchUpdate(retVal);
        }
        return retVal;
    }

    private void batchUpdate(List<World> worlds) {
        dao.save(worlds);
    }

    private World findAndModifyOne(int id) {
        World world = dao.findById(id);
        notFoundIfNull(world);
        int newNumber;
        do {
            newNumber = randomWorldNumber();
        } while (newNumber == world.randomNumber);
        world.randomNumber = newNumber;
        return BATCH_SAVE ? world : dao.save(world);
    }

    private static int randomWorldNumber() {
        return ThreadLocalRandom.current().nextInt(WORLD_MAX_ROW) + 1;
    }

    private static int regulateQueries(String param) {
        if (null == param) {
            return 1;
        }
        try {
            int val = Integer.parseInt(param, 10);
            return val < 1 ? 1 : val > 500 ? 500 : val;
        } catch (NumberFormatException e) {
            return 1;
        }
    }
}
