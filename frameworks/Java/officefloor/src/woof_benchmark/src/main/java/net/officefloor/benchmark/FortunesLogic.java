package net.officefloor.benchmark;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.EntityManager;

/**
 * Fortunes logic.
 */
public class FortunesLogic {

	public Fortune[] getFortunes(EntityManager entityManager) {
		List<Fortune> fortunes = new ArrayList<>();
		entityManager.createNamedQuery("All", Fortune.class).getResultStream()
				.forEach((fortune) -> fortunes.add(fortune));
		fortunes.add(new Fortune(0, "Additional fortune added at request time."));
		fortunes.sort((a, b) -> a.getMessage().compareTo(b.getMessage()));
		return fortunes.toArray(new Fortune[fortunes.size()]);
	}

}
