package benchmark.repository;

import benchmark.model.Fortune;
import benchmark.model.World;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

public class JDBCDbService implements DbService {

    @Override
    public List<World> getWorld(int num) {

        String select = "select id, randomNumber from World where id = ?";
        List<World> worldList = new ArrayList<>();

        try (Connection conn = JDBCConnectionFactory.INSTANCE.getConnection();
             PreparedStatement pstm = conn.prepareStatement(select)) {

            for (int randomId : getRandomNumberSet(num)) {
                pstm.setInt(1, randomId);
                try (ResultSet rs = pstm.executeQuery()) {
                    rs.next();
                    World world = new World();
                    world.setId(rs.getInt("id"));
                    world.setRandomNumber(rs.getInt("randomNumber"));
                    worldList.add(world);
                }
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }

        return worldList;
    }

    @Override
    public List<Fortune> getFortune() {

        String select = "select id, message from Fortune";
        List<Fortune> fortuneList = new ArrayList<>();

        try (Connection conn = JDBCConnectionFactory.INSTANCE.getConnection();
             PreparedStatement pstm = conn.prepareStatement(select);
             ResultSet rs = pstm.executeQuery()) {

            while (rs.next()) {
                Fortune fortune = new Fortune();
                fortune.setId(rs.getInt("id"));
                fortune.setMessage(rs.getString("message"));
                fortuneList.add(fortune);
            }
            fortuneList.add(new Fortune(defaultFortuneId, defaultFortuneMessage));
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }

        fortuneList.sort(Comparator.comparing(Fortune::getMessage));
        return fortuneList;
    }

    @Override
    public List<World> updateWorld(int num) {

        String update = "update World set randomNumber = ? where id = ?";
        List<World> worldList = getWorld(num);

        try (Connection conn = JDBCConnectionFactory.INSTANCE.getConnection();
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
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }

        return worldList;
    }
}
