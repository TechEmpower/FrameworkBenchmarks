<?php namespace Dev;
/**
 * Dev ship
 **
 * 
 * @package       Dev
 * @author        Mario Döring <mario@clancats.com>
 * @version       1.0
 * @copyright     2010 - 2014 ClanCats GmbH
 *
 */
class Ship extends \CCOrbit_Ship
{
	/**
	 * initialize the ship
	 * 
	 * @return void
	 */
	public function wake()
	{
		if ( !\ClanCats::in_development() ) 
		{
			return;
		}
		
		// get all controllers in the dev namespace
		foreach( \CCFile::ls( \CCPath::controllers( 'Dev::*Controller'.EXT ) ) as $path ) 
		{
			$name = \CCStr::cut( basename( $path ), 'Controller'.EXT );
			\CCRouter::on( 'dev/'.\CCStr::lower( $name ), 'Dev::'.$name );
		}
	}
}
