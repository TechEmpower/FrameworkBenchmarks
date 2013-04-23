package models;

import play.modules.siena.EnhancedModel;
import siena.Column;
import siena.Generator;
import siena.Id;

import play.modules.siena.EnhancedModel;
import siena.Generator;

public class World extends EnhancedModel  {

	public World(long i, long number) {
		id = i;
		randomNumber = new Long(number);
	}
	
	public World() {
	}

	@Id(Generator.NONE)
	public Long id ;
	
	@Column("randomNumber")
	public Long randomNumber;

}