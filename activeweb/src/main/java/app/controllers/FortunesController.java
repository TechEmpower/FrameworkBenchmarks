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
 * @author Igor Polevoy: 12/18/13 9:11 PM
 */

package app.controllers;

import app.models.Fortune;
import org.javalite.activeweb.AppController;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

public class FortunesController extends AppController {

    public void index() {
        List<Fortune> dbFortunes = Fortune.findAll();
        List<Fortune> fortunes = new ArrayList<Fortune>(dbFortunes);
        fortunes.add((Fortune) Fortune.create("id", 0, "message", "New fortune: " + new Date().toString()));
        Collections.sort(fortunes);
        view("fortunes", fortunes);
        render().noLayout();
    }
}
