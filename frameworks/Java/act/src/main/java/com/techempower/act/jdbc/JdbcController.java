package com.techempower.act.jdbc;

import act.app.DbServiceManager;
import act.controller.Controller;
import act.db.sql.SqlDbService;
import com.techempower.act.beetlsql.Fortune;
import com.techempower.act.beetlsql.World;
import org.osgl.mvc.annotation.GetAction;
import org.osgl.mvc.result.Result;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static com.techempower.act.controller.WorldControllerBase.randomWorldNumber;
import static com.techempower.act.controller.WorldControllerBase.regulateQueries;

/**
 * Testing for Act on raw JDBC
 */
@Controller("jdbc")
@SuppressWarnings("unused")
public class JdbcController extends Controller.Util {

    private Connection conn(DbServiceManager dsm) throws SQLException {
        SqlDbService pgsql = dsm.dbService("pgsql");
        return pgsql.dataSource().getConnection();
    }

    @GetAction("db")
    public final void singleQuery(DbServiceManager dsm) throws SQLException {
        try (Connection conn = conn(dsm)) {
            throw json(findOne(query(conn)));
        }
    }

    @GetAction("queries")
    public final Result multipleQueries(String queries, DbServiceManager dsm) throws SQLException {
        int q = regulateQueries(queries);

        World[] worlds = new World[q];
        try (Connection conn = conn(dsm)) {
            PreparedStatement query = query(conn);
            for (int i = 0; i < q; ++i) {
                worlds[i] = findOne(query);
            }
        }
        return json(worlds);
    }

    @GetAction("updates")
    public final Result updateQueries(String queries, DbServiceManager dsm) throws SQLException {
        int count = regulateQueries(queries);
        World[] worlds = new World[count];
        try (Connection conn = conn(dsm)) {
            try (PreparedStatement query = query(conn);
                 PreparedStatement update = update(conn)) {
                for (int i = 0; i < count; i++) {
                    final int id = randomWorldNumber();
                    query.setInt(1, id);

                    try (ResultSet results = query.executeQuery()) {
                        if (results.next()) {
                            worlds[i] = new World(id, results.getInt("randomNumber"));
                            worlds[i].setRandomNumber(randomWorldNumber());
                            update.setInt(1, worlds[i].getRandomNumber());
                            update.setInt(2, id);
                            update.execute();
                        }
                    }
                }
            }
        }
        return json(worlds);
    }

    @GetAction("fortunes")
    public void fortunes(DbServiceManager dsm) throws SQLException {
        List<Fortune> fortunes = new ArrayList<>();
        try (Connection conn = conn(dsm)) {
            ResultSet set = conn.createStatement(ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)
                    .executeQuery("SELECT id, message FROM Fortune");
            while (set.next()) {
                fortunes.add(new Fortune(set.getInt(1), set.getString(2)));
            }
        }
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes);
        template("fortunes.mustache", fortunes);
    }

    private World findOne(PreparedStatement statement) throws SQLException {
        World world;
        statement.setInt(1, randomWorldNumber());
        try (ResultSet resultSet = statement.executeQuery()) {
            resultSet.next();
            return new World(resultSet.getInt("id"), resultSet.getInt("randomNumber"));
        }
    }

    private PreparedStatement query(Connection conn) throws SQLException {
        return conn.prepareStatement("SELECT * FROM world WHERE id = ?", ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY);
    }

    private PreparedStatement update(Connection conn) throws SQLException {
        return conn.prepareStatement("UPDATE World SET randomNumber = ? WHERE id= ?");
    }


}
