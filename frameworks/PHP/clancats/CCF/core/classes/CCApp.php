<?php namespace Core;
/**
 * App object 
 * The application object implements some events, in the most
 * cases your routes are app specific so return them in your App class.
 ** 
 *
 * @package		ClanCatsFramework
 * @author		Mario Döring <mario@clancats.com>
 * @version		2.0
 * @copyright 	2010 - 2014 ClanCats GmbH
 *
 */
class CCApp 
{	
	/**
	 * The application name
	 *
	 * @var string
	 */
	public static $name = 'CCF Application';
	
	/**
	 * Get the applications name
	 *
	 * @return string
	 */
	public static function name() 
	{
		return static::$name;
	}
	
	/**
	 * This function should provide the application routes.
	 * By default its going to use the router.config file.
	 *
	 * @return array
	 */
	public static function routes() 
	{
		return CCConfig::create( ClanCats::$config->get( 'router.map' ) )->raw();
	}
	
	/**
	 * Application initialization.
	 *
	 * do your inital stuff here like getting the current user object ect..
	 * You can return a CCResponse wich will cancle all 
	 * other actions if its enebaled ( see. main.config -> send_app_wake_response )
	 *
	 * @return void | CCResponse
	 */
	public static function wake() 
	{
		// do stuff
	}
}