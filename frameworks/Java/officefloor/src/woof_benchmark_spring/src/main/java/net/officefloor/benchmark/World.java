package net.officefloor.benchmark;

import javax.persistence.Entity;
import javax.persistence.Id;

import lombok.Data;

@Data
@Entity
public class World {

	@Id
	private int id;

	private int randomNumber;
}