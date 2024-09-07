package hello;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import hello.web.DbHandler;
import hello.web.WebmvcRouter;
import hello.model.World;
import hello.repository.DbRepository;

@Service
public class UpdateWorldServiceImpl implements UpdateWorldService {

	private DbRepository dbRepository;
	
	public UpdateWorldServiceImpl(DbRepository dbRepository) {
		this.dbRepository = dbRepository;
	}

	@Override
	@Transactional
	public World updateWorld(int worldId) {
		var world = dbRepository.getWorld(worldId);
		// Ensure that the new random number is not equal to the old one.
		// That would cause the JPA-based implementation to avoid sending the
		// UPDATE query to the database, which would violate the test
		// requirements.

		// Locally the records doesn't exist, maybe in the yours is ok but we need to
		// make this check
		if (world == null) {
			return null;
		}

		int newRandomNumber;
		do {
			newRandomNumber = DbHandler.randomWorldNumber();
		} while (newRandomNumber == world.randomnumber);

		return dbRepository.updateWorld(world, newRandomNumber);
	}

}
