package benchmark;

import benchmark.model.Fortune;
import benchmark.model.World;
import benchmark.repository.FortuneRepository;
import jakarta.inject.Singleton;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

@Singleton
public class JdbcFortuneRepository implements FortuneRepository {

    private final DataSource dataSource;

    public JdbcFortuneRepository(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    @Override
    public void initDb(Collection<Fortune> fortunes) {
        try (Connection connection = dataSource.getConnection()) {
            connection.createStatement().execute("DROP TABLE IF EXISTS Fortune;");
            connection.createStatement().execute("CREATE TABLE Fortune (id INTEGER NOT NULL,message VARCHAR(255) NOT NULL);");
            try (PreparedStatement statement = connection.prepareStatement("INSERT INTO fortune VALUES (?, ?);")) {
                for (Fortune fortune : fortunes) {
                    statement.setInt(1, fortune.id());
                    statement.setString(2, fortune.message());
                    statement.addBatch();
                }
                statement.executeBatch();
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public List<Fortune> findAll() {
        try (Connection connection = dataSource.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement("SELECT id, message FROM fortune",
                    ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
                List<Fortune> fortunes = new ArrayList<>();
                try (ResultSet resultSet = statement.executeQuery()) {
                    while (resultSet.next()) {
                        fortunes.add(new Fortune(resultSet.getInt(1), resultSet.getString(2)));
                    }
                }
                return fortunes;
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

}
