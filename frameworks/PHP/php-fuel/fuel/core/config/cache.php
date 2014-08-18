<?php
/**
 * Part of the Fuel framework.
 *
 * @package    Fuel
 * @version    1.5
 * @author     Fuel Development Team
 * @license    MIT License
 * @copyright  2010 - 2013 Fuel Development Team
 * @link       http://fuelphp.com
 */

/**
 * NOTICE:
 *
 * If you need to make modifications to the default configuration, copy
 * this file to your app/config folder, and make them in there.
 *
 * This will allow you to upgrade fuel without losing your custom config.
 */


return array(

	/**
	 * ----------------------------------------------------------------------
	 * global settings
	 * ----------------------------------------------------------------------
	 */

	// default storage driver
	'driver'      => 'file',

	// default expiration (null = no expiration)
	'expiration'  => null,

	/**
	 * Default content handlers: convert values to strings to be stored
	 * You can set them per primitive type or object class like this:
	 *   - 'string_handler' 		=> 'string'
	 *   - 'array_handler'			=> 'json'
	 *   - 'Some_Object_handler'	=> 'serialize'
	 */

	/**
	 * ----------------------------------------------------------------------
	 * storage driver settings
	 * ----------------------------------------------------------------------
	 */

	// specific configuration settings for the file driver
	'file'  => array(
		'path'  =>	'',  // if empty the default will be application/cache/
	),

	// specific configuration settings for the memcached driver
	'memcached'  => array(
		'cache_id'  => 'fuel',  // unique id to distinquish fuel cache items from others stored on the same server(s)
		'servers'   => array(   // array of servers and portnumbers that run the memcached service
			'default' => array('host' => '127.0.0.1', 'port' => 11211, 'weight' => 100)
		),
	),

	// specific configuration settings for the apc driver
	'apc'  => array(
		'cache_id'  => 'fuel',  // unique id to distinquish fuel cache items from others stored on the same server(s)
	),

	// specific configuration settings for the redis driver
	'redis'  => array(
		'database'  => 'default'  // name of the redis database to use (as configured in config/db.php)
	),
);


