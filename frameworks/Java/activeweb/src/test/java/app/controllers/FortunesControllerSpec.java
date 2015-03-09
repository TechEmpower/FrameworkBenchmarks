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

import org.javalite.activeweb.DBControllerSpec;
import org.junit.Ignore;
import org.junit.Test;

/**
 * @author Eric Nielsen
 */
public class FortunesControllerSpec extends DBControllerSpec {

    @Test
    public void shouldRenderHtml() {
        request().integrateViews().get("index");
        System.out.print(responseContent());
        the(responseContent()).shouldContain(
                "<tr><td>11</td><td>&lt;script&gt;alert(&quot;This should not be displayed in a browser alert box.&quot;);&lt;/script&gt;</td></tr>"
                + "<tr><td>4</td><td>A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1</td></tr>"
                + "<tr><td>5</td><td>A computer program does what you tell it to do, not what you want it to do.</td></tr>"
                + "<tr><td>2</td><td>A computer scientist is someone who fixes things that aren&apos;t broken.</td></tr>");
    }

    @Ignore
    @Test
    public void shouldRenderHtmlOneMinute() {
        long endMillis = System.currentTimeMillis() + 60*1000;
        do {
            request().integrateViews().get("index");
            responseContent();
        } while (System.currentTimeMillis() < endMillis);
    }
}
