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
 * @author Eric Nielsen
 */
public class PlaintextControllerSpec extends ControllerSpec {

    @Test
    public void shouldRenderResponse() {
        request().get("index");
        the(responseContent()).shouldBeEqual("Hello, World!");
    }

    @Ignore
    public void shouldRenderHtmlOneMinute() {
        long endMillis = System.currentTimeMillis() + 60*1000;
        do {
            request().get("index");
            responseContent();
        } while (System.currentTimeMillis() < endMillis);
    }
}
