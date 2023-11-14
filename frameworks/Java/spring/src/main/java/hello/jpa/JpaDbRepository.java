package hello.jpa;

import java.util.List;

import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

import hello.model.Fortune;
import hello.model.World;
import hello.repository.DbRepository;

@Service
@Profile("jpa")
public class JpaDbRepository implements DbRepository {
	private final WorldRepository worldRepository;
	private final FortuneRepository fortuneRepository;

	public JpaDbRepository(WorldRepository worldRepository, FortuneRepository fortuneRepository) {
		this.worldRepository = worldRepository;
		this.fortuneRepository = fortuneRepository;
	}

	@Override
	public World getWorld(int id) {
		return worldRepository.findById(id).orElse(null);
	}

	@Override
	public World updateWorld(World world, int randomNumber) {
		world.randomnumber = randomNumber;
		return worldRepository.save(world);
	}

	@Override
	public List<Fortune> fortunes() {
		return fortuneRepository.findAll();
	}
}
