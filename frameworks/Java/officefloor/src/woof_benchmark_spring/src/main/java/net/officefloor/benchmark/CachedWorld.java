package net.officefloor.benchmark;

import javax.persistence.Entity;
import javax.persistence.Id;

import lombok.Data;

@Data
@Entity
public class CachedWorld {

	@Id
	private int id;

	private int randomNumber;
}