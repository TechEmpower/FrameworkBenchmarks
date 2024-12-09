package hello.repository;

import hello.model.Fortune;
import hello.model.World;
import org.noear.solon.annotation.Component;
import org.noear.solon.annotation.Inject;
import org.noear.solon.data.sql.SqlUtils;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

@Component
public class JdbcDbRepository implements DbRepository {
    @Inject
    SqlUtils sqlUtils;

    @Override
    public World getWorld(int id) {
        try {
            return sqlUtils.sql("SELECT id, randomnumber FROM world WHERE id = ?", id)
                    .queryRow()
                    .toBean(World.class, (r, t) -> new World((int) r.getObject(1), (int) r.getObject(2)));
        } catch (Exception e) {
            return null;
        }
    }

    @Override
    public void updateWorlds(List<World> worlds) throws SQLException {
        List<Object[]> values = new ArrayList<>();
        for (World w : worlds) {
            values.add(new Object[]{w.randomNumber, w.id});
        }

        sqlUtils.sql("UPDATE world SET randomnumber = ? WHERE id = ?")
                .updateBatch(values);
    }

    @Override
    public List<Fortune> fortunes() throws SQLException {
        return sqlUtils.sql("SELECT id, message FROM fortune")
                .queryRowList()
                .toBeanList(Fortune.class, (r, t) -> new Fortune((int) r.getObject(1), (String) r.getObject(2)));
    }
}
