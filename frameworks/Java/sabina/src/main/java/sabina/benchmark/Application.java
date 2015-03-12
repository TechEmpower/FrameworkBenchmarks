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
import static sabina.Sabina.*;
import static sabina.content.JsonContent.toJson;
import static sabina.view.MustacheView.renderMustache;

import com.mchange.v2.c3p0.ComboPooledDataSource;
import sabina.Request;

import java.sql.*;
import java.util.*;
import java.util.Date;
import java.util.concurrent.ThreadLocalRandom;

import javax.sql.DataSource;

/**
 * .
 */
final class Application {
    private static final String SETTINGS_RESOURCE = "/server.properties";
    private static final Properties SETTINGS = loadConfiguration ();

    private static final String JDBC_URL = SETTINGS.getProperty ("mysql.uri")
        .replace ("${db.host}", "localhost"); // TODO Move this to Gradle build
    private static final DataSource DATA_SOURCE = createSessionFactory ();
    private static final int DB_ROWS = 10000;

    private static final boolean AUTOCOMMIT = getProperty ("sabina.benchmark.autocommit") != null;
    private static final String SELECT_WORLD = "select * from world where id = ?";
    private static final String UPDATE_WORLD = "update world set randomNumber = ? where id = ?";
    private static final String SELECT_FORTUNES = "select * from fortune";

    private static final String MESSAGE = "Hello, World!";
    private static final String CONTENT_TYPE_TEXT = "text/plain";
    private static final String CONTENT_TYPE_JSON = "application/json";
    private static final String QUERIES_PARAM = "queries";

    private static Properties loadConfiguration () {
        try {
            Properties settings = new Properties ();
            settings.load (Class.class.getResourceAsStream (SETTINGS_RESOURCE));
            return settings;
        }
        catch (Exception ex) {
            throw new RuntimeException (ex);
        }
    }

    private static DataSource createSessionFactory () {
        try {
            ComboPooledDataSource dataSource = new ComboPooledDataSource ();
            dataSource.setMinPoolSize (32);
            dataSource.setMaxPoolSize (256);
            dataSource.setCheckoutTimeout (1800);
            dataSource.setMaxStatements (50);
            dataSource.setJdbcUrl (JDBC_URL);
            return dataSource;
        }
        catch (Exception ex) {
            throw new RuntimeException (ex);
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

    private static Object getJson (Request it) {
        it.response.type (CONTENT_TYPE_JSON);
        return toJson (new Message ());
    }

    private static Object getDb (Request it) {
        final int queries = getQueries (it);
        final World[] worlds = new World[queries];

        try (final Connection con = DATA_SOURCE.getConnection ()) {
            final Random random = ThreadLocalRandom.current ();
            final PreparedStatement stmt = con.prepareStatement (SELECT_WORLD);

            for (int ii = 0; ii < queries; ii++) {
                stmt.setInt (1, random.nextInt (DB_ROWS) + 1);
                final ResultSet rs = stmt.executeQuery ();
                while (rs.next ())
                    worlds[ii] = new World (rs.getInt (1), rs.getInt (2));
            }
        }
        catch (SQLException e) {
            e.printStackTrace ();
        }

        it.response.type (CONTENT_TYPE_JSON);
        return toJson (it.queryParams (QUERIES_PARAM) == null? worlds[0] : worlds);
    }

    private static Object getFortunes (Request it) {
        final List<Fortune> fortunes = new ArrayList<> ();

        try (final Connection con = DATA_SOURCE.getConnection ()) {
            final ResultSet rs = con.prepareStatement (SELECT_FORTUNES).executeQuery ();
            while (rs.next ())
                fortunes.add (new Fortune (rs.getInt (1), rs.getString (2)));
        }
        catch (SQLException e) {
            e.printStackTrace ();
        }

        fortunes.add (new Fortune (0, "Additional fortune added at request time."));
        fortunes.sort ((a, b) -> a.message.compareTo (b.message));

        it.response.type ("text/html; charset=utf-8");
        return renderMustache ("/fortunes.mustache", fortunes);
    }

    private static Object getUpdates (Request it) {
        final int queries = getQueries (it);
        final World[] worlds = new World[queries];

        try (final Connection con = DATA_SOURCE.getConnection ()) {
            con.setAutoCommit (AUTOCOMMIT);

            final Random random = ThreadLocalRandom.current ();
            final PreparedStatement stmtSelect = con.prepareStatement (SELECT_WORLD);
            final PreparedStatement stmtUpdate = con.prepareStatement (UPDATE_WORLD);

            for (int ii = 0; ii < queries; ii++) {
                stmtSelect.setInt (1, random.nextInt (DB_ROWS) + 1);
                final ResultSet rs = stmtSelect.executeQuery ();
                while (rs.next ()) {
                    worlds[ii] = new World (rs.getInt (1), rs.getInt (2));
                    stmtUpdate.setInt (1, random.nextInt (DB_ROWS) + 1);
                    stmtUpdate.setInt (2, worlds[ii].id);

                    if (AUTOCOMMIT) {
                        stmtUpdate.executeUpdate ();
                    }
                    else {
                        stmtUpdate.addBatch ();
                    }
                }
            }

            if (!AUTOCOMMIT) {
                int count = 0;
                boolean retrying;

                do {
                    try {
                        stmtUpdate.executeBatch ();
                        retrying = false;
                    }
                    catch (BatchUpdateException e) {
                        retrying = true;
                    }
                }
                while (count++ < 10 && retrying);

                con.commit ();
            }
        }
        catch (SQLException e) {
            e.printStackTrace ();
        }

        it.response.type (CONTENT_TYPE_JSON);
        return toJson (it.queryParams (QUERIES_PARAM) == null? worlds[0] : worlds);
    }

    private static Object getPlaintext (Request it) {
        it.response.type (CONTENT_TYPE_TEXT);
        return MESSAGE;
    }

    private static void addCommonHeaders (Request it) {
        it.header ("Server", "Undertow/1.1.2");
        it.response.raw ().addDateHeader ("Date", new Date ().getTime ());
    }

    public static void main (String[] args) {
        get ("/json", Application::getJson);
        get ("/db", Application::getDb);
        get ("/query", Application::getDb);
        get ("/fortune", Application::getFortunes);
        get ("/update", Application::getUpdates);
        get ("/plaintext", Application::getPlaintext);
        after (Application::addCommonHeaders);

        host (SETTINGS.getProperty ("web.host"));
        port (SETTINGS.getProperty ("web.port"));
        start ();
    }
}
