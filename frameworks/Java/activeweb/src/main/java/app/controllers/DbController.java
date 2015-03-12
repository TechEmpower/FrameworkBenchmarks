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

import app.models.World;
import java.io.IOException;
import org.javalite.activeweb.AppController;

import java.util.concurrent.ThreadLocalRandom;

/**
 * @author Igor Polevoy: 12/18/13 4:36 PM
 * @author Eric Nielsen
 */
public class DbController extends AppController {
    public void index() {
        respond(World.findById(randomNumber()).toJson(false, "id", "randomNumber")).contentType("application/json");
    }

    public void jackson() throws IOException {
        JsonController.WRITER.writeValue(outputStream("application/json"), World.findById(randomNumber()));
    }

    protected int randomNumber(){
        return ThreadLocalRandom.current().nextInt(10000) + 1;
    }
}
