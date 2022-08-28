package hello.repository;

import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Profile;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

import hello.model.Fortune;
import hello.model.World;

@Repository
@Profile("jdbc")
public class JdbcDbRepository implements DbRepository {
	private final Logger log = LoggerFactory.getLogger(getClass());
	private final JdbcTemplate jdbcTemplate;

	public JdbcDbRepository(JdbcTemplate jdbcTemplate) {
		this.jdbcTemplate = jdbcTemplate;
	}

	@Override
	public World getWorld(int id) {
		log.debug("getWorld({})", id);
		try {
			return jdbcTemplate.queryForObject("SELECT * FROM world WHERE id = ?",
					(rs, rn) -> new World(rs.getInt("id"), rs.getInt("randomnumber")), id);
		} catch (EmptyResultDataAccessException e) {
			return null;
		}
	}

	private World updateWorld(World world) {
		jdbcTemplate.update("UPDATE world SET randomnumber = ? WHERE id = ?", world.randomnumber, world.id);
		return world;
	}

	@Override
	public World updateWorld(World world, int randomNumber) {
		world.randomnumber = randomNumber;
		return updateWorld(world);
	}

	@Override
	public List<Fortune> fortunes() {
		return jdbcTemplate.query("SELECT * FROM fortune",
				(rs, rn) -> new Fortune(rs.getInt("id"), rs.getString("message")));
	}
}
