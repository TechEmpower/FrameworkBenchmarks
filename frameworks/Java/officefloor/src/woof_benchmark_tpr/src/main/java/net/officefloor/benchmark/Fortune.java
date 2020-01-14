package net.officefloor.benchmark;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class Fortune implements Comparable<Fortune> {

	private int id;

	private String message;

	public String getMessage() {
		return this.message;
	}

	@Override
	public int compareTo(Fortune o) {
		return this.getMessage().compareTo(o.getMessage());
	}
}