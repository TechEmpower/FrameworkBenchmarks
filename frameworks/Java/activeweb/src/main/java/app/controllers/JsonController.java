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

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import org.javalite.activeweb.AppController;

import java.util.Map;
import java.util.TreeMap;

/**
 * @author Igor Polevoy: 12/18/13 3:51 PM
 * @author Eric Nielsen
 */
public class JsonController extends AppController {
    static final ObjectWriter WRITER = new ObjectMapper().writer();

    private Map<String, Object> newMessage() {
        Map<String, Object> message = new TreeMap<String, Object>();
        message.put("message", "Hello, World!");
        return message;
    }

    public void index() {
        view("message", newMessage());
        render().noLayout().contentType("application/json");
    }

    public void jackson() throws JsonProcessingException {
        respond(WRITER.writeValueAsString(newMessage())).contentType("application/json");
    }
}
