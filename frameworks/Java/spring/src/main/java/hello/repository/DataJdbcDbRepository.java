package hello.repository;

import java.util.List;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Repository;

import hello.model.Fortune;
import hello.model.World;

@Repository
@Profile("data-jdbc")
public class DataJdbcDbRepository implements DbRepository {

	private WorldDataJdbcRepository worldDataJdbcRepository;
	private FortuneDataJdbcRepository fortuneDataJdbcRepository;

	public DataJdbcDbRepository(WorldDataJdbcRepository worldDataJdbcRepository, FortuneDataJdbcRepository fortuneDataJdbcRepository) {
		this.worldDataJdbcRepository = worldDataJdbcRepository;
		this.fortuneDataJdbcRepository = fortuneDataJdbcRepository;
	}

	@Override
	public World getWorld(int id) {
		return worldDataJdbcRepository.findById(id).orElse(null);
	}

	@Override
	public void updateWorlds(List<World> worlds) {
		worldDataJdbcRepository.saveAll(worlds);
	}

	@Override
	public List<Fortune> fortunes() {
		return fortuneDataJdbcRepository.findAll();
	}

}