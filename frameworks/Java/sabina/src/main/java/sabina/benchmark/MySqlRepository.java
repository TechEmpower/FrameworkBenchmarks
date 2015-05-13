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

import static java.lang.System.getProperty;
import static sabina.benchmark.Application.DB_ROWS;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

import javax.sql.DataSource;

import com.mchange.v2.c3p0.ComboPooledDataSource;

final class MySqlRepository implements Repository {
    private static final boolean AUTOCOMMIT = getProperty ("sabina.benchmark.autocommit") != null;
    private static final String SELECT_WORLD = "select * from world where id = ?";
    private static final String UPDATE_WORLD = "update world set randomNumber = ? where id = ?";
    private static final String SELECT_FORTUNES = "select * from fortune";

    private final DataSource DATA_SOURCE;

    MySqlRepository (Properties settings) {
        final String jdbcUrl = settings.getProperty ("mysql.uri");
        DATA_SOURCE = createSessionFactory (jdbcUrl);
    }

    private DataSource createSessionFactory (String jdbcUrl) {
        try {
            ComboPooledDataSource dataSource = new ComboPooledDataSource ();
            dataSource.setMinPoolSize (32);
            dataSource.setMaxPoolSize (256);
            dataSource.setCheckoutTimeout (1800);
            dataSource.setMaxStatements (50);
            dataSource.setJdbcUrl (jdbcUrl);
            return dataSource;
        }
        catch (Exception ex) {
            throw new RuntimeException (ex);
        }
    }

    private void commitUpdate (Connection con, PreparedStatement stmtUpdate)
        throws SQLException {
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

    private void updateWorld (World world, PreparedStatement stmtUpdate)
        throws SQLException {
        stmtUpdate.setInt (1, world.randomNumber);
        stmtUpdate.setInt (2, world.id);

        if (AUTOCOMMIT) {
            stmtUpdate.executeUpdate ();
        }
        else {
            stmtUpdate.addBatch ();
        }
    }

    @Override public List<Fortune> getFortunes () {
        final List<Fortune> fortunes = new ArrayList<> ();

        try (final Connection con = DATA_SOURCE.getConnection ()) {
            final ResultSet rs = con.prepareStatement (SELECT_FORTUNES).executeQuery ();
            while (rs.next ())
                fortunes.add (new Fortune (rs.getInt (1), rs.getString (2)));
        }
        catch (SQLException e) {
            e.printStackTrace ();
        }

        return fortunes;
    }

    @Override public World[] getWorlds (int queries, boolean update) {
        final World[] worlds = new World[queries];

        try (final Connection con = DATA_SOURCE.getConnection ()) {
            if (update)
                con.setAutoCommit (AUTOCOMMIT);

            final Random random = ThreadLocalRandom.current ();
            final PreparedStatement stmtSelect = con.prepareStatement (SELECT_WORLD);
            final PreparedStatement stmtUpdate = update? con.prepareStatement (UPDATE_WORLD) : null;

            for (int ii = 0; ii < queries; ii++) {
                stmtSelect.setInt (1, random.nextInt (DB_ROWS) + 1);
                final ResultSet rs = stmtSelect.executeQuery ();
                while (rs.next ()) {
                    worlds[ii] = new World (rs.getInt (1), rs.getInt (2));

                    if (update) {
                        worlds[ii] = new World (worlds[ii].id, random.nextInt (DB_ROWS) + 1);
                        updateWorld (worlds[ii], stmtUpdate);
                    }
                }
            }

            if (update && !AUTOCOMMIT)
                commitUpdate (con, stmtUpdate);
        }
        catch (SQLException e) {
            e.printStackTrace ();
        }

        return worlds;
    }
}
