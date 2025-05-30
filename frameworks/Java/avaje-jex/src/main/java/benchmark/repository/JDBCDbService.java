package benchmark.repository;

import benchmark.model.Fortune;
import benchmark.model.World;
import jakarta.inject.Singleton;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import javax.sql.DataSource;

@Singleton
public class JDBCDbService implements DbService {

  private final DataSource datasource;

  public JDBCDbService(DataSource connectionFactory) {
    this.datasource = connectionFactory;
  }

  @Override
  public List<World> getWorld(int num) {

    String select = "select id, randomNumber from World where id = ?";
    List<World> worldList = new ArrayList<>();

    try (Connection conn = datasource.getConnection();
        PreparedStatement pstm = conn.prepareStatement(select)) {

      for (int randomId : getRandomNumberSet(num)) {
        pstm.setInt(1, randomId);
        try (ResultSet rs = pstm.executeQuery()) {
          rs.next();
          worldList.add(new World(rs.getInt("id"), rs.getInt("randomNumber")));
        }
      }
    } catch (SQLException e) {
      throw new RuntimeException(e);
    }

    return worldList;
  }

  @Override
  public List<Fortune> getFortune() throws SQLException {

    String select = "select id, message from Fortune";
    List<Fortune> fortuneList = new ArrayList<>();

    try (Connection conn = datasource.getConnection();
        PreparedStatement pstm = conn.prepareStatement(select);
        ResultSet rs = pstm.executeQuery()) {

      while (rs.next()) {
        fortuneList.add(new Fortune(rs.getInt("id"), rs.getString("message")));
      }
      fortuneList.add(new Fortune(defaultFortuneId, defaultFortuneMessage));
    }

    fortuneList.sort(Comparator.comparing(Fortune::message));
    return fortuneList;
  }

  @Override
  public List<World> updateWorld(int num) throws SQLException {

    String update = "update World set randomNumber = ? where id = ?";
    List<World> worldList = getWorld(num);

    try (Connection conn = datasource.getConnection();
        PreparedStatement pstm = conn.prepareStatement(update)) {

      conn.setAutoCommit(false);
      for (World world : worldList) {
        int newRandomNumber;
        do {
          newRandomNumber = getRandomNumber();
        } while (newRandomNumber == world.getRandomNumber());

        pstm.setInt(1, newRandomNumber);
        pstm.setInt(2, world.getId());
        pstm.addBatch();

        world.setRandomNumber(newRandomNumber);
      }
      pstm.executeBatch();
      conn.commit();
    }

    return worldList;
  }
}
