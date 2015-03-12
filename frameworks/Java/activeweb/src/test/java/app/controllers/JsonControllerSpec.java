/*
Copyright 2009-2015 Igor Polevoy

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
package app.controllers;

import org.javalite.activeweb.ControllerSpec;
import org.junit.Ignore;
import org.junit.Test;


/**
 * @author Igor Polevoy: 12/18/13 3:59 PM
 * @author Eric Nielsen
 */
public class JsonControllerSpec extends ControllerSpec {

    @Test
    public void shouldRenderMessage() {
        request().integrateViews().get("index");
        the(responseContent()).shouldBeEqual("{\"message\":\"Hello, World!\"}");
        the(contentType()).shouldBeEqual("application/json");
    }

    @Test
    public void shouldRenderMessageWithJackson() {
        request().get("jackson");
        the(responseContent()).shouldBeEqual("{\"message\":\"Hello, World!\"}");
        the(contentType()).shouldBeEqual("application/json");
    }

    @Ignore
    @Test
    public void shouldRenderMessageOneMinute() {
        long endMillis = System.currentTimeMillis() + 60*1000;
        do {
            request().integrateViews().get("index");
            responseContent();
        } while (System.currentTimeMillis() < endMillis);
    }
}
