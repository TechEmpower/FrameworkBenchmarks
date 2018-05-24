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
import act.util.FastJsonFeature;
import act.util.Global;
import act.util.JsonView;
import com.alibaba.fastjson.serializer.SerializerFeature;
import com.techempower.act.AppEntry;
import com.techempower.act.model.World;
import org.osgl.mvc.annotation.GetAction;
import org.osgl.mvc.annotation.SessionFree;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import javax.inject.Inject;

@AutoConfig
@Env.RequireProfile(value = AppEntry.PROFILE_JSON_PLAINTEXT, except = true)
@JsonView
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
    @SessionFree
    public World findOne() {
        return dao.findById(randomWorldNumber());
    }

    @GetAction("queries")
    @SessionFree
    @FastJsonFeature(SerializerFeature.DisableCircularReferenceDetect)
    public final World[] multipleQueries(String queries) {
        int q = regulateQueries(queries);

        World[] worlds = new World[q];
        for (int i = 0; i < q; ++i) {
            worlds[i] = findOne();
        }
        return worlds;
    }

    @GetAction("updates")
    @SessionFree
    @FastJsonFeature(SerializerFeature.DisableCircularReferenceDetect)
    public final List<World> updateQueries(String queries) {
        int q = regulateQueries(queries);
        return doUpdate(q);
    }

    @Transactional
    private List<World> doUpdate(int q) {
        List<World> retVal = new ArrayList<>(q);
        for (int i = 0; i < q; ++i) {
            retVal.add(findAndModifyOne());
        }
        if (BATCH_SAVE) {
            batchUpdate(retVal);
        }
        return retVal;
    }

    private void batchUpdate(List<World> worlds) {
        dao.save(worlds);
    }

    private World findAndModifyOne() {
        World world = findOne();
        notFoundIfNull(world);
        world.randomNumber = randomWorldNumber();
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
