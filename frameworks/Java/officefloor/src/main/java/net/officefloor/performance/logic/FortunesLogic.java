package net.officefloor.performance.logic;

import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

import javax.persistence.EntityManager;

import lombok.Data;
import net.officefloor.performance.entities.Fortune;

/**
 * Logic for Database Update query.
 * 
 * @author Daniel Sagenschneider
 */
public class FortunesLogic {

	@Data
	public static class TemplateData {
		private final Fortune[] fortunes;
	}

	public TemplateData getTemplate(EntityManager entityManager) {

		// Obtain all the fortunes
		List<Fortune> list = entityManager.createNamedQuery(
				Fortune.NAMED_QUERY_ALL, Fortune.class).getResultList();

		// Obtain the fortunes as array
		Fortune[] fortunes = list.toArray(new Fortune[list.size() + 1]);
		int index = 0;
		for (Fortune fortune : list) {
			fortunes[index++] = fortune;
		}

		// Add the necessary Fortune
		fortunes[fortunes.length - 1] = new Fortune(
				"Additional fortune added at request time.");

		// Sort the fortunes
		Arrays.sort(fortunes, new Comparator<Fortune>() {
			@Override
			public int compare(Fortune a, Fortune b) {
				return String.CASE_INSENSITIVE_ORDER.compare(a.getMessage(),
						b.getMessage());
			}
		});

		// Return the data for the template
		return new TemplateData(fortunes);
	}

}