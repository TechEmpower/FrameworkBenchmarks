package io.helidon.benchmark.models;

import io.reactivex.Scheduler;
import io.reactivex.Single;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.ArrayList;
import java.util.List;

public class JdbcRepository implements DbRepository {
    private final DataSource dataSource;
    private final Scheduler scheduler;

    public JdbcRepository(DataSource dataSource, Scheduler scheduler) {
        this.dataSource = dataSource;
        this.scheduler = scheduler;
    }

    @Override
    public Single<World> getWorld(int id) {
        return Single.fromCallable(() -> {
            try (Connection connection = dataSource.getConnection()) {
                    PreparedStatement statement = connection.prepareStatement("SELECT id, randomnumber FROM world WHERE id = ?");
                    statement.setInt(1, id);
                    ResultSet rs = statement.executeQuery();
                    rs.next();
                    World world = new World(rs.getInt(1), rs.getInt(2));
                    statement.close();
                    return world;
            }
        }).subscribeOn(this.scheduler);
    }

    @Override
    public Single<World> updateWorld(World world) {
        return Single.fromCallable(() -> {
                    try (Connection connection = dataSource.getConnection()) {
                        PreparedStatement statement = connection.prepareStatement("UPDATE world SET randomnumber = ? WHERE id = ?");
                        statement.setInt(1, world.randomNumber);
                        statement.setInt(2, world.id);
                        statement.execute();
                        statement.close();
                        return world;
                    }
        }).subscribeOn(this.scheduler);
    }

    @Override
    public Single<List<Fortune>> getFortunes() {
        return Single.fromCallable(() -> {
            try (Connection connection = dataSource.getConnection()) {
                PreparedStatement statement = connection.prepareStatement("SELECT id, message FROM fortune");
                ResultSet rs = statement.executeQuery();

                List<Fortune> fortunes = new ArrayList<>();
                while (rs.next()) {
                    fortunes.add(new Fortune(rs.getInt(1), rs.getString(2)));
                }
                statement.close();
                return fortunes;
            }
        }).subscribeOn(this.scheduler);
    }
}
