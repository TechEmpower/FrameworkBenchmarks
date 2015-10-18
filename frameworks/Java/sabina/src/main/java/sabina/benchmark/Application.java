/*
 * Copyright © 2015 Juan José Aguililla. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the
 * License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language governing permissions
 * and limitations under the License.
 */

package sabina.benchmark;

import static java.lang.Integer.parseInt;
import static java.lang.System.getProperty;
import static sabina.content.JsonContent.toJson;
import static sabina.view.MustacheView.renderMustache;

import sabina.Request;

import java.util.*;
import java.util.Date;
import javax.servlet.annotation.WebFilter;

@WebFilter ("/*")
public final class Application extends sabina.Application {
    static final String SETTINGS_RESOURCE = "/server.properties";
    static final int DB_ROWS = 10000;

    private static final String MESSAGE = "Hello, World!";
    private static final String CONTENT_TYPE_TEXT = "text/plain";
    private static final String CONTENT_TYPE_JSON = "application/json";
    private static final String QUERIES_PARAM = "queries";

    static Repository repository = loadRepository ();

    static Properties loadConfiguration () {
        try {
            Properties settings = new Properties ();
            settings.load (Application.class.getResourceAsStream (SETTINGS_RESOURCE));
            return settings;
        }
        catch (Exception ex) {
            throw new RuntimeException (ex);
        }
    }

    static Repository loadRepository () {
        switch (getProperty ("sabina.benchmark.repository", "mysql")) {
            case "mongodb":
                return new MongoDbRepository (loadConfiguration ());
            case "mysql":
            default:
                return new MySqlRepository (loadConfiguration ());
        }
    }

    private static Object getDb (Request it) {
        try {
            final World[] worlds = repository.getWorlds (getQueries (it), false);
            it.response.type (CONTENT_TYPE_JSON);
            return toJson (it.queryParams (QUERIES_PARAM) == null? worlds[0] : worlds);
        }
        catch (Exception e) {
            e.printStackTrace ();
            return e.getMessage ();
        }
    }

    private static Object getFortunes (Request it) {
        try {
            List<Fortune> fortunes = repository.getFortunes ();
            fortunes.add (new Fortune (0, "Additional fortune added at request time."));
            fortunes.sort ((a, b) -> a.message.compareTo (b.message));

            it.response.type ("text/html; charset=utf-8");
            return renderMustache ("fortunes.mustache", fortunes);
        }
        catch (Exception e) {
            e.printStackTrace ();
            return e.getMessage ();
        }
    }

    private static Object getUpdates (Request it) {
        try {
            World[] worlds = repository.getWorlds (getQueries (it), true);
            it.response.type (CONTENT_TYPE_JSON);
            return toJson (it.queryParams (QUERIES_PARAM) == null? worlds[0] : worlds);
        }
        catch (Exception e) {
            e.printStackTrace ();
            return e.getMessage ();
        }
    }

    private static int getQueries (final Request request) {
        try {
            String parameter = request.queryParams (QUERIES_PARAM);
            if (parameter == null)
                return 1;

            int queries = parseInt (parameter);
            if (queries < 1)
                return 1;
            if (queries > 500)
                return 500;

            return queries;
        }
        catch (NumberFormatException ex) {
            return 1;
        }
    }

    private static Object getPlaintext (Request it) {
        it.response.type (CONTENT_TYPE_TEXT);
        return MESSAGE;
    }

    private static Object getJson (Request it) {
        it.response.type (CONTENT_TYPE_JSON);
        return toJson (new Message ());
    }

    private static void addCommonHeaders (Request it) {
        it.header ("Server", "Undertow/1.1.2");
        it.response.addDateHeader ("Date", new Date ().getTime ());
    }

    public Application () {
        get ("/json", Application::getJson);
        get ("/db", Application::getDb);
        get ("/query", Application::getDb);
        get ("/fortune", Application::getFortunes);
        get ("/update", Application::getUpdates);
        get ("/plaintext", Application::getPlaintext);
        after (Application::addCommonHeaders);

        Properties settings = loadConfiguration ();

        bind (settings.getProperty ("web.host"));
        port (parseInt (settings.getProperty ("web.port")));

        start ();
    }

    public static void main (String[] args) {
        new Application ();
    }

//    @Override public void init (FilterConfig filterConfig) {
//        // Web always uses Mongo because connection pool configuration problems
//        repository = new MongoDbRepository (loadConfiguration ());
//
//        get ("/json", Application::getJson);
//        get ("/db", Application::getDb);
//        get ("/query", Application::getDb);
//        get ("/fortune", Application::getFortunes);
//        get ("/update", Application::getUpdates);
//        get ("/plaintext", Application::getPlaintext);
//        after (Application::addCommonHeaders); // TODO Is this required inside a server?
//    }
}
