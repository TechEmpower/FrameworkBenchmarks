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

import act.Act;
import act.job.OnAppStart;
import act.sys.Env;
import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.serializer.SerializerFeature;
import com.techempower.act.AppEntry;
import org.osgl.http.H;

@Env.RequireProfile(value = AppEntry.PROFILE_JSON_PLAINTEXT)
public class HelloWorldController {

    private static final String HELLO_WORLD = "Hello, World!";
    private static final String JSON_TYPE = H.Format.JSON.contentType();

    public static final class Message {
        private final String message;
        private Message(String message) {
            this.message = message;
        }
        public String getMessage() {
            return message;
        }
    }

    @OnAppStart
    public void routing() {
        Act.getNonblock("/json", context -> context.resp()
                .contentType(JSON_TYPE)
                .writeContent(JSON.toJSONString(new Message(HELLO_WORLD), SerializerFeature.DisableCircularReferenceDetect)));
    }

}
