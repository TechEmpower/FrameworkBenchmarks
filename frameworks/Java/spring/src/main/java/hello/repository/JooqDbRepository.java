package hello.repository;

import static hello.db.tables.Fortune.FORTUNE;
import static hello.db.tables.World.WORLD;

import java.util.List;

import org.jooq.DSLContext;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Repository;

import hello.db.tables.records.WorldRecord;
import hello.model.Fortune;
import hello.model.World;

@Repository
@Profile("jooq")
public class JooqDbRepository implements DbRepository {

	private DSLContext dslContext;

	public JooqDbRepository(DSLContext dslContext) {
		this.dslContext = dslContext;
	}

	@Override
	public World getWorld(int id) {
		return dslContext.selectFrom(WORLD).where(WORLD.ID.eq(id)).fetchOneInto(World.class);
	}

	@Override
	public void updateWorlds(List<World> worlds) {
		dslContext.batchUpdate(worlds.stream().map(it -> new WorldRecord(it.id, it.randomNumber)).toList()).execute();
	}

	@Override
	public List<Fortune> fortunes() {
		return dslContext.selectFrom(FORTUNE).fetchInto(Fortune.class);
	}

}