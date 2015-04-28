<?php namespace CCUnit;
/**
 * CCUnit Ship
 *
 * @package       CCUnit
 * @author        Mario Döring <mario@clancats.com>
 * @version       1.0.0
 * @copyright     2010 - 2014 ClanCats GmbH
 */
class Ship extends \CCOrbit_Ship 
{
	/**
	 * install the ship
	 */
	public function install() 
	{
		throw new \Exception( "Cannot install CCUnit ship it comes with the core." );
	}
	
	/**
	 * uninstall the ship
	 */
	public function uninstall() 
	{
		throw new \Exception( "Cannot remove CCUnit ship it comes with the core." );
	}
}
