package net.officefloor.performance.entities;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;

import net.officefloor.plugin.web.http.template.UnescapedHtml;
import lombok.Data;
import lombok.NonNull;

/**
 * Fortune entity.
 * 
 * @author Daniel Sagenschneider
 */
@Data
@Entity
@NamedQueries({ @NamedQuery(name = "Fortune.getAll", query = "SELECT e FROM Fortune e") })
public class Fortune {
	public static final String NAMED_QUERY_ALL = "Fortune.getAll";

	@Id
	private int id;
	
	@NonNull
	private String message;
}