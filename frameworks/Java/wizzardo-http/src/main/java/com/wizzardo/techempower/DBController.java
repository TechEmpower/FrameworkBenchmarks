package com.wizzardo.techempower;

import com.wizzardo.http.framework.Controller;
import com.wizzardo.http.framework.parameters.Parameter;
import com.wizzardo.http.framework.template.Renderer;
import com.wizzardo.http.request.Header;

import java.nio.charset.StandardCharsets;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

public class DBController extends Controller {

    DBService dbService;

    public Renderer world() throws SQLException {
        World world;
        try (Connection connection = dbService.getConnection();
             PreparedStatement statement = connection.prepareStatement("SELECT id,randomNumber FROM World WHERE id = ?")
        ) {
            statement.setInt(1, getRandomNumber());
            try (ResultSet resultSet = statement.executeQuery()) {
                resultSet.next();
                int id = resultSet.getInt(1);
                int randomNumber = resultSet.getInt(2);
                world = new World(id, randomNumber);
            }
        }

        return renderJson(world);
    }

    public Renderer queries() throws SQLException {
        int queries = Math.min(Math.max(params().getInt("queries", 1), 1), 500);

        World[] worlds = new World[queries];
        try (Connection connection = dbService.getConnection();
             PreparedStatement statement = connection.prepareStatement("SELECT id,randomNumber FROM World WHERE id = ?")
        ) {
            for (int i = 0; i < worlds.length; i++) {
                statement.setInt(1, getRandomNumber());
                try (ResultSet resultSet = statement.executeQuery()) {
                    resultSet.next();
                    int id = resultSet.getInt(1);
                    int randomNumber = resultSet.getInt(2);
                    worlds[i] = new World(id, randomNumber);
                }
            }
        }

        return renderJson(worlds);
    }

    public Renderer updates() throws SQLException {
        int queries = Math.min(Math.max(params().getInt("queries", 1), 1), 500);
        World[] worlds = new World[queries];
        try (Connection connection = dbService.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement("SELECT id,randomNumber FROM World WHERE id = ?")) {
                for (int i = 0; i < worlds.length; i++) {
                    statement.setInt(1, getRandomNumber());
                    try (ResultSet resultSet = statement.executeQuery()) {
                        resultSet.next();
                        int id = resultSet.getInt(1);
                        int randomNumber = resultSet.getInt(2);
                        worlds[i] = new World(id, randomNumber);
                    }
                }
            }
            try (PreparedStatement statement = connection.prepareStatement("UPDATE World SET randomNumber = ? WHERE id = ?")) {
                for (World world : worlds) {
                    world.randomNumber = getRandomNumber();
                    statement.setInt(1, world.randomNumber);
                    statement.setInt(2, world.id);
                    statement.executeUpdate();
                }
            }
        }
        return renderJson(worlds);
    }

    protected int getRandomNumber() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }

    public static final class World {
        public int id;
        public int randomNumber;

        public World(int id, int randomNumber) {
            this.id = id;
            this.randomNumber = randomNumber;
        }
    }
}
