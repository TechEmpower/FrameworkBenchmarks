package org.smartboot.http;

import org.smartboot.http.server.handle.HttpHandle;
import org.smartboot.http.utils.NumberUtils;

import javax.sql.DataSource;
import java.io.IOException;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.concurrent.ThreadLocalRandom;

/**
 * @author 三刀
 * @version V1.0 , 2020/6/16
 */
public class UpdateHandler extends HttpHandle {
    private DataSource dataSource;

    public UpdateHandler(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    @Override
    public void doHandle(HttpRequest httpRequest, HttpResponse response) throws IOException {
        try (Connection connection = dataSource.getConnection();
             PreparedStatement queryPreparedStatement = connection.prepareStatement("SELECT * FROM World WHERE id=?");
             PreparedStatement preparedStatement = connection.prepareStatement("UPDATE world SET randomnumber=? WHERE id=?");) {
            int queries = Math.min(Math.max(NumberUtils.toInt(httpRequest.getParameter("queries"), 1), 1), 500);
            World[] worlds = new World[queries];

            for (int i = 0; i < queries; i++) {
                queryPreparedStatement.setInt(1, getRandomNumber());
                ResultSet resultSet = queryPreparedStatement.executeQuery();
                resultSet.next();
                World world = new World();
                world.setId(resultSet.getInt(1));
                world.setRandomNumber(resultSet.getInt(2));
                worlds[i] = world;
                worlds[i].setRandomNumber(getRandomNumber());
                queryPreparedStatement.clearParameters();
            }

            for (int i = 0; i < queries; i++) {
                preparedStatement.setInt(1, worlds[i].getRandomNumber());
                preparedStatement.setInt(2, worlds[i].getId());
                preparedStatement.addBatch();
            }
            preparedStatement.executeBatch();
            response.setContentType("application/json");
            JsonUtil.writeJsonBytes(response, worlds);
        } catch (SQLException throwables) {
            throwables.printStackTrace();
        }
    }

    protected int getRandomNumber() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }
}
