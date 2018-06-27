package net.officefloor.benchmark;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class World implements Comparable<World> {

	private int id;

	private int randomNumber;

	@Override
	public int compareTo(World o) {
		return this.id - o.id;
	}
}