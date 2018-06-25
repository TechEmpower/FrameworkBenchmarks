package net.officefloor.benchmark;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class Fortune {

	private int id;

	private String message;
	
	public String getMessage() {
		return this.message;
	}
}