package org.smartboot.http;

import org.smartboot.http.common.utils.NumberUtils;
import org.smartboot.http.server.HttpRequest;
import org.smartboot.http.server.HttpResponse;
import org.smartboot.http.server.HttpServerHandler;

import javax.sql.DataSource;
import java.io.IOException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.StringJoiner;
import java.util.concurrent.ThreadLocalRandom;

/**
 * @author 三刀
 * @version V1.0 , 2020/6/16
 */
public class UpdateHandler extends HttpServerHandler {
    private DataSource dataSource;

    public UpdateHandler(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    @Override
    public void handle(HttpRequest httpRequest, HttpResponse response) throws IOException {
        int queries = Math.min(Math.max(NumberUtils.toInt(httpRequest.getParameter("queries"), 1), 1), 500);
        World[] worlds = new World[queries];
        StringJoiner updateSql = new StringJoiner(
                ", ",
                "UPDATE world SET randomnumber = temp.randomnumber FROM (VALUES ",
                " ORDER BY 1) AS temp(id, randomnumber) WHERE temp.id = world.id");
        try (Connection connection = dataSource.getConnection()) {

            try (PreparedStatement queryPreparedStatement = connection.prepareStatement("SELECT * FROM World WHERE id=?");) {
                for (int i = 0; i < queries; i++) {
                    queryPreparedStatement.setInt(1, getRandomNumber());
                    ResultSet resultSet = queryPreparedStatement.executeQuery();
                    resultSet.next();
                    World world = new World();
                    world.setId(resultSet.getInt(1));
                    world.setRandomNumber(getRandomNumber());
                    worlds[i] = world;
                    queryPreparedStatement.clearParameters();
                    updateSql.add("(?, ?)");
                }
            }

            try (PreparedStatement preparedStatement = connection.prepareStatement(updateSql.toString());) {
                int i = 0;
                for (World world : worlds) {
                    preparedStatement.setInt(++i, world.getId());
                    preparedStatement.setInt(++i, world.getRandomNumber());
                }
                preparedStatement.executeUpdate();
            }

        } catch (SQLException throwables) {
            throwables.printStackTrace();
        }
        response.setContentType("application/json");
        JsonUtil.writeJsonBytes(response, worlds);
    }

    protected int getRandomNumber() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }
}
