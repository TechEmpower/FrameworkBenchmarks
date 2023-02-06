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
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ThreadLocalRandom;

/**
 * @author 三刀
 * @version V1.0 , 2020/6/16
 */
public class MultipleQueriesHandler extends HttpServerHandler {
    private DataSource dataSource;
    private ExecutorService executorService;

    public MultipleQueriesHandler(DataSource dataSource, ExecutorService executorService) {
        this.dataSource = dataSource;
        this.executorService = executorService;
    }

    @Override
    public void handle(HttpRequest request, HttpResponse response, CompletableFuture<Object> completableFuture) throws IOException {
        executorService.execute(() -> {
            try {
                int queries = Math.min(Math.max(NumberUtils.toInt(request.getParameter("queries"), 1), 1), 500);
                World[] worlds = new World[queries];
                try (Connection connection = dataSource.getConnection();
                     PreparedStatement preparedStatement = connection.prepareStatement("SELECT * FROM World WHERE id=?");) {

                    for (int i = 0; i < queries; i++) {
                        preparedStatement.setInt(1, getRandomNumber());
                        ResultSet resultSet = preparedStatement.executeQuery();
                        resultSet.next();
                        World world = new World();
                        world.setId(resultSet.getInt(1));
                        world.setRandomNumber(resultSet.getInt(2));
                        worlds[i] = world;
                        preparedStatement.clearParameters();
                    }

                } catch (SQLException throwables) {
                    throwables.printStackTrace();
                }
                response.setContentType("application/json");
                JsonUtil.writeJsonBytes(response, worlds);
            } finally {
                completableFuture.complete(null);
            }
        });
    }

    protected int getRandomNumber() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }
}
