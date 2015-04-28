<?php namespace Packtacular;
/**
 * Packtacular ship
 **
 * 
 * @package		Packtacular
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
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
		// wrap the assets
		\CCFinder::alias( 'CCAsset', __DIR__.'/CCAsset'.EXT );
		
		// map the other classes
		\CCFinder::package( __DIR__.'/', array(
			'Packtacular'			=> 'Packtacular'.EXT,
			'Packtacular\\Theme'		=> 'Theme'.EXT,
			'lessc'					=> 'lib/lessc.inc'.EXT,
		));
		
		// add writeable directory hook
		\CCEvent::mind( 'ccdoctor.permissions', function() {
			return PUBLICPATH.\CCConfig::create( "Packtacular::packtacular" )->path;
		});
	}
}
