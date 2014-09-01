<?php namespace Core;
/**
 * Server Input
 * Server data including client information POST / GET params ect...
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCServer extends CCIn 
{	
	/**
	 * The initial call of the CCServer get all Superglobals
	 * and assigns them to itself as an holder.
	 * 
	 * @return void
	 */
	public static function _init() 
	{	
		// create new instance from default input
		CCServer::$_instance = CCIn::create( $_GET, $_POST, $_COOKIE, $_FILES, $_SERVER );
		
		// unset default http holder to safe mem
		//unset( $_GET, $_POST, $_COOKIE, $_SERVER, $_FILES );
	}
}