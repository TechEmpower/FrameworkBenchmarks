package net.officefloor.benchmark;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.NamedQuery;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@NamedQuery(name = "All", query = "SELECT f FROM Fortune f")
public class Fortune {

	@Id
	private int id;

	private String message;
	
	public String getMessage() {
		return this.message;
	}
}