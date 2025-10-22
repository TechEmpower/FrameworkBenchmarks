package hello.repository;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

import org.springframework.context.annotation.Profile;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.ParameterizedPreparedStatementSetter;
import org.springframework.stereotype.Repository;

import hello.model.Fortune;
import hello.model.World;

@Repository
@Profile("jdbc")
public class JdbcDbRepository implements DbRepository {
	private final JdbcTemplate jdbcTemplate;

	public JdbcDbRepository(JdbcTemplate jdbcTemplate) {
		this.jdbcTemplate = jdbcTemplate;
	}

	@Override
	public World getWorld(int id) {
		try {
			return jdbcTemplate.queryForObject("SELECT id, randomnumber FROM world WHERE id = ?",
					(rs, rn) -> new World(rs.getInt(1), rs.getInt(2)), id);
		} catch (EmptyResultDataAccessException e) {
			return null;
		}
	}

	@Override
	public void updateWorlds(List<World> worlds) {
		jdbcTemplate.batchUpdate("UPDATE world SET randomnumber = ? WHERE id = ?", worlds, worlds.size(), new ParameterizedPreparedStatementSetter<World>() {
			@Override
			public void setValues(PreparedStatement ps, World world) throws SQLException {
				ps.setInt(1, world.randomNumber);
				ps.setInt(2, world.id);
			}
		});
	}

	@Override
	public List<Fortune> fortunes() {
		return jdbcTemplate.query(con -> con.prepareStatement("SELECT id, message FROM fortune",
				ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY), rs -> {
			List<Fortune> results = new ArrayList<>();
			while (rs.next()) {
				results.add(new Fortune(rs.getInt(1), rs.getString(2)));
			}
			return results;
		});
	}

}
