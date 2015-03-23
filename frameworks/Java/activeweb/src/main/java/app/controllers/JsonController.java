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

import app.models.Message;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
import java.io.IOException;
import org.javalite.activeweb.AppController;


/**
 * @author Igor Polevoy: 12/18/13 3:51 PM
 * @author Eric Nielsen
 */
public class JsonController extends AppController {
    static final ObjectWriter WRITER = new ObjectMapper().writer();

    public void index() {
        view("message", new Message("Hello, World!"));
        render("/json/index").noLayout().contentType("application/json");
    }

    public void jackson() throws IOException {
        WRITER.writeValue(outputStream("application/json"), new Message("Hello, World!"));
    }
}
