/*
Copyright 2009-2010 Igor Polevoy 

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

/**
 * @author Igor Polevoy: 12/18/13 3:59 PM
 */

package app.controllers;

import org.javalite.activeweb.ControllerSpec;
import org.junit.Test;

import java.util.Map;

public class JsonControllerSpec extends ControllerSpec {

    @Test
    public void shouldRenderMessage() {

        //execute controller
        request().get("index");

        //process result
        Map result = JsonHelper.toMap(responseContent());

        //test result
        a(result.get("message")).shouldBeEqual("Hello, World!");
        a(contentType()).shouldBeEqual("application/json");
    }
}
