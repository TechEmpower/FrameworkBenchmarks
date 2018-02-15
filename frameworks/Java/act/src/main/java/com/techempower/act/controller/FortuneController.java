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

import static act.controller.Controller.Util.renderTemplate;

import act.db.Dao;
import act.sys.Env;
import act.util.Global;
import act.view.NoImplicitTemplateVariable;
import com.techempower.act.AppEntry;
import com.techempower.act.model.Fortune;
import org.osgl.mvc.annotation.GetAction;
import org.osgl.mvc.annotation.SessionFree;

import java.util.Collections;
import java.util.List;
import javax.inject.Inject;

@Env.RequireProfile(value = AppEntry.PROFILE_JSON_PLAINTEXT, except = true)
public class FortuneController {

    @Global
    @Inject
    private Dao<Integer, Fortune, ?> dao;

    @GetAction("fortunes")
    @NoImplicitTemplateVariable
    @SessionFree
    public void fortunes() {
        List<Fortune> fortunes = dao.findAllAsList();
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes);
        renderTemplate("fortunes", fortunes);
    }

}
