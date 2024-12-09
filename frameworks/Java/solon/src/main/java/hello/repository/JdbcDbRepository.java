package hello.repository;

import hello.model.Fortune;
import hello.model.World;
import org.noear.solon.annotation.Component;
import org.noear.solon.annotation.Inject;
import org.noear.solon.data.sql.SqlUtils;

import java.sql.SQLException;
import java.util.List;

@Component
public class JdbcDbRepository implements DbRepository {
    @Inject
    SqlUtils sqlUtils;

    @Override
    public World getWorld(int id) {
        try {
            return sqlUtils.sql("SELECT id, randomnumber FROM world WHERE id = ?", id)
                    .queryRow((rs) -> new World(rs.getInt(1), rs.getInt(2)));
        } catch (Exception e) {
            return null;
        }
    }

    @Override
    public void updateWorlds(List<World> worlds) throws SQLException {
        sqlUtils.sql("UPDATE world SET randomnumber = ? WHERE id = ?")
                .updateBatch(worlds, (ps, w) -> {
                    ps.setInt(1, w.randomNumber);
                    ps.setInt(2, w.id);
                });
    }

    @Override
    public List<Fortune> fortunes() throws SQLException {
        return sqlUtils.sql("SELECT id, message FROM fortune")
                .queryRowList((r) -> new Fortune(r.getInt(1), r.getString(2)));
    }
}
