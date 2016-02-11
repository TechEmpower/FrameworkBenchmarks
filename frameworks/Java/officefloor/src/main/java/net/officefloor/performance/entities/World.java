package net.officefloor.performance.entities;

import javax.persistence.Entity;
import javax.persistence.Id;

import lombok.Data;

/**
 * World entity.
 * 
 * @author Daniel Sagenschneider
 */
@Data
@Entity
public class World {
	@Id
	private int id;
	private int randomNumber;
}