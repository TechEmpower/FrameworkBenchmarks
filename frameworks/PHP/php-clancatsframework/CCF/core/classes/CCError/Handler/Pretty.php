<?php namespace Core;
/**
 * Pretty error handler
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 * 
 */
class CCError_Handler_Pretty extends CCError_Handler
{
	/**
	 * respond information to the user
	 * 
	 * @return void
	 */
	public function respond()
	{
		$inspector = $this->inspector;
		require __DIR__."/resources/error_page".EXT;
	}
}