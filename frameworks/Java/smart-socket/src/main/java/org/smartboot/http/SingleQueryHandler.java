package org.smartboot.http;

import org.smartboot.http.server.handle.HttpHandle;

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
public class SingleQueryHandler extends HttpHandle {
    private DataSource dataSource;

    public SingleQueryHandler(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    @Override
    public void doHandle(HttpRequest httpRequest, HttpResponse response) throws IOException {
        try (Connection connection = dataSource.getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement("SELECT * FROM World WHERE id=?");) {
            preparedStatement.setInt(1, getRandomNumber());
            ResultSet resultSet = preparedStatement.executeQuery();
            resultSet.next();
            World world = new World();
            world.setId(resultSet.getInt(1));
            world.setRandomNumber(resultSet.getInt(2));
            response.setContentType("application/json");
            JsonUtil.writeJsonBytes(response, world);
        } catch (SQLException throwables) {
            throwables.printStackTrace();
        }
    }

    protected int getRandomNumber() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }
}
